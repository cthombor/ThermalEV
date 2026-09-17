#' fit_model: uses optim() to find a best-fit of the non-resistive params,
#' with a MSE criterion
#'
#' @param m a thmodel
#' @param heat_capacity in kJ/K
#' @param polarisation_rev in kJ/V
#' @param polarisation_irr dimensionless, irreversible heat of polarisation
#' @param lambda_module_to_ambient in hours
#' @param lambda_module_AC_to_ambient in hours
#' @param fan_power in W
#' @param COP coefficient of heatpump performance, dimensionless
#' @param effective_pack_resistance in mOhms, not optimised
#' @param packr85 in mOhms, not optimised
#' @param arrhenius_resistance in K, not optimised
#' @param iter_count controls convergence on predicted temps
#' @param min_segment_length shorter sequences of samples are ignored
#' @param fixed_parameters length-6 Boolean vector, reduces dimension of opt
#' @param trace 0 for silent, 1 for minimal, 2 for verbose
#' @param from_date starting date/time (for a time-restricted optimisation)
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#'
#' @returns modified thmodel, with best-fit predictions and parameters
#' @export
#'
#' @examples
#' m <- fit_model(thmodels = list("eNV200ac50kWh"))
fit_model <- function(
    m = NULL,
    arrhenius_resistance = NA,
    heat_capacity = NA,
    polarisation_rev = NA,
    polarisation_irr = NA,
    lambda_module_to_ambient = NA,
    lambda_module_AC_to_ambient = NA,
    fan_power = NA,
    COP = NA,
    effective_pack_resistance = NA,
    packr85 = NA,
    iter_count = 4,
    min_segment_length = 20,
    fixed_parameters = c(F, F, F, F, F, F, F),
    trace = 1,
    from_date = NULL,
    to_date = NULL,
    from_idx = NULL,
    to_idx = NULL) {

#' fm: local fcn, interface to predict_temp(), for use by optim()
#'
#' Side effect: updates thmodel `m` in the calling environment
#'
#' @param x parameter list
#'
  fm <- function(x = c(hc, prev, pirr, lp, la, fanp, COP)) {
    m <- predict_temp(
      m,
      heat_capacity = x[1],
      polarisation_rev = x[2],
      polarisation_irr = x[3],
      lambda_module_to_ambient = x[4],
      lambda_module_AC_to_ambient = x[5],
      fan_power = x[6],
      COP = x[7],
      trace = trace
    )
    return(MSE_of_fit(m))
  }

  if (is.null(m)) m <- munge_logfile()  # use our default logfile

  if (length(m$parameters) == 0) {
    m <- default_params(m)
  }
  stopifnot(length(m$parameters) == 11)

  stopifnot(length(fixed_parameters) == 7)

  # param values specified in the method call have precedence. Side effect:
  # if m$parameters is malformed, throw a "subscript out of bounds" error
  if (!is.na(heat_capacity)) {
    m$parameters[["heat_capacity"]] <- heat_capacity
  }
  if (!is.na(polarisation_rev)) {
    m$parameters[["polarisation_rev"]] <- polarisation_rev
  }
  if (!is.na(polarisation_irr)) {
    m$parameters[["polarisation_irr"]] <- polarisation_irr
  }
  if (!is.na(lambda_module_to_ambient)) {
    m$parameters[["lambda_module_to_ambient"]] <- lambda_module_to_ambient
  }
  if (!is.na(lambda_module_AC_to_ambient)) {
    m$parameters[["lambda_module_AC_to_ambient"]] <- lambda_module_AC_to_ambient
  }
  if (!is.na(fan_power)) {
    m$parameters[["lambda_cooling_power"]] <- fan_power
  }
  if (!is.na(COP)) {
    m$parameters[["COP"]] <- COP
  }
  if (!is.na(arrhenius_resistance)) {
    m$parameters[["arrhenius_resistance"]] <- arrhenius_resistance
  }
  if (!is.na(effective_pack_resistance)) {
    m$parameters[["effective_pack_resistance"]] <- effective_pack_resistance
  }
  if (!is.na(packr85)) {
    m$parameters[["packr85"]] <- packr85
  }

  # read a full set of primary factors into shorthand vars
  hc <- m$parameters[["heat_capacity"]]
  prev <- m$parameters[["polarisation_rev"]]
  pirr <- m$parameters[["polarisation_irr"]]
  lp <- m$parameters[["lambda_module_to_ambient"]]
  la <- m$parameters[["lambda_module_AC_to_ambient"]]
  fanp <- m$parameters[["fan_power"]]
  COP <- m$parameters[["COP"]]
  arr <- m$parameters[["arrhenius_resistance"]]
  packr <- m$parameters[["effective_pack_resistance"]]
  r85 <- m$parameters[["packr85"]]

  # all logs "should" be sorted on date-time... but just in case...
  plotdata <- m$logdata |> arrange(date_time)
  # curiously, xts insists on UTC for stored dates & times
  from_idx <- ifelse(is.null(from_date),
                     ifelse(is.null(from_idx), 1, from_idx),
                     dplyr::first(which(
                       plotdata$date_time >= as.POSIXct(from_date, tz = "UTC")
                     )))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                   dplyr::last(which(
                     plotdata$date_time <= as.POSIXct(to_date, tz = "UTC")
                   )))
  if (is.na(from_idx) || is.na(to_idx)) {
    warning("Date out of range")
  } else if (from_idx >= to_idx) {
    warning("from_date is not before to_date")
  }

  orig_model <- m
  m$logdata <- m$logdata |> slice(from_idx:to_idx) # restricted range model

  # n.b. the box-constrained optimisation of L-BFGS-B throws an error if any
  # dimension of the box is zero, so we add an epsilon and hope for the best
  bestfit <- optim(
    par = c(hc, prev, pirr, lp, la, fanp, COP),
    fn = fm,
    lower = c(if (fixed_parameters[1]) hc else 200,
              if (fixed_parameters[2]) prev else -50,
              if (fixed_parameters[3]) pirr else 0,
              if (fixed_parameters[4]) lp else 0,
              if (fixed_parameters[5]) la else 0,
              if (fixed_parameters[6]) fanp else 0,
              if (fixed_parameters[7]) COP else 0.1),
    upper = c(if (fixed_parameters[1]) hc + 1 else 400,
              if (fixed_parameters[2]) prev + 0.1 else 50,
              if (fixed_parameters[3]) pirr + 0.01 else 2,
              if (fixed_parameters[4]) lp + 0.1 else 15,
              if (fixed_parameters[5]) la + 0.1 else 10,
              if (fixed_parameters[6]) fanp + 10 else 600,
              if (fixed_parameters[7]) COP + 0.1 else 6),
    control = list(maxit = iter_count,
                   ndeps = c(1, 0.1, 0.01, 0.1, 0.1, 10, 0.1)),
    method = "L-BFGS-B")

  # remove epsilons from the best-fit of fixed parameters
  best_hc = ifelse (fixed_parameters[1], hc, bestfit$par[1])
  best_prev = ifelse (fixed_parameters[2], prev, bestfit$par[2])
  best_pirr = ifelse (fixed_parameters[3], pirr, bestfit$par[3])
  best_lp = ifelse (fixed_parameters[3], lp, bestfit$par[4])
  best_la = ifelse (fixed_parameters[4], la, bestfit$par[5])
  best_fanp = ifelse (fixed_parameters[5], fanp, bestfit$par[6])
  best_COP = ifelse (fixed_parameters[6], COP, bestfit$par[7])

  # evaluate predict_temp(m) on the best_fit parameters
  # n.b. the fit will be degraded by any epsilon-shifts in sensitive params
  m <- predict_temp(
    m,
    heat_capacity = best_hc,
    polarisation_rev = best_prev,
    polarisation_irr = best_pirr,
    lambda_module_to_ambient = best_lp,
    lambda_module_AC_to_ambient = best_la,
    fan_power = best_fanp,
    COP = best_COP,
    arrhenius_resistance = arr,
    effective_pack_resistance = packr,
    packr85 = r85,
    iter_count = iter_count,
    min_segment_length = min_segment_length,
    trace = ifelse(trace == 0, 0, trace + 1)
  )

  if ((to_idx - from_idx + 1) < length(orig_model$logdata$err_pred)) {
    cat("MSE of fit over the specified range:", round(MSE_of_fit(m), 3), "\n")
    # evaluate predict_temp() on the best_fit parameters, full model
    m <- predict_temp(
      orig_model,
      heat_capacity = best_hc,
      polarisation_rev = best_prev,
      polarisation_irr = best_pirr,
      lambda_module_to_ambient = best_lp,
      lambda_module_AC_to_ambient = best_la,
      fan_power = best_fanp,
      COP = best_COP,
      arrhenius_resistance = arr,
      effective_pack_resistance = packr,
      packr85 = r85,
      iter_count = iter_count,
      min_segment_length = min_segment_length,
      trace = ifelse(trace == 0, 0, trace + 1)
    )
  }
  cat("MSE of fit over the full model:", round(MSE_of_fit(m) , 3), "\n")

  return(m)
}
