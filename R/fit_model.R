#' fit_model: uses optim() to find a best-fit, with MSE criterion
#'
#' @param m a thmodel
#' @param effective_pack_resistance in mOhms
#' @param polarisation_energy in kJ/V
#' @param lambda_module_to_ambient in hours
#' @param lambda_module_to_ambient in hours
#' @param fan_power in W
#' @param COP coefficient of heatpump performance, dimensionless
#' @param arrhenius_resistance in K
#' @param heat_capacity in kJ/K
#' @param iter_count controls convergence on predicted temps
#' @param min_segment_length shorter sequences of samples are ignored
#' @param fixed_parameters length-8 Boolean vector, reduces dimension of opt
#' @param trace 0 for silent, 1 for minimal, 2 for verbose
#' @param from_date starting date/time
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
    effective_pack_resistance = NA,
    polarisation_energy = NA,
    lambda_module_to_ambient = NA,
    lambda_module_AC_to_ambient = NA,
    fan_power = NA,
    COP = NA,
    arrhenius_resistance = NA,
    heat_capacity = NA,
    iter_count = 4,
    min_segment_length = 20,
    fixed_parameters = c(T, F, F, F, F, F, F, F),
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
  fm <- function(x = c(packr, pe, lp, la, fanp, COP, arr, hc)) {
    m <- predict_temp(
      m,
      effective_pack_resistance = x[1],
      polarisation_energy = x[2],
      lambda_module_to_ambient = x[3],
      lambda_module_AC_to_ambient = x[4],
      fan_power = x[5],
      COP = x[6],
      arrhenius_resistance = x[7],
      heat_capacity = x[8],
      trace = trace
    )
    return(MSE_of_fit(m))
  }

  if (is.null(m)) m <- munge_logfile()  # use our default logfile

  stopifnot((length(m$parameters) == 0) || (length(m$parameters) == 9))
  if (length(m$parameters) == 0) {
    m <- default_params(m)
  }

  # param values specified in the method call have precedence. Side effect:
  # if m$parameters is malformed, throw a "subscript out of bounds" error
  if (!is.na(effective_pack_resistance)) {
    m$parameters[["effective_pack_resistance"]] <- effective_pack_resistance
  }
  if (!is.na(polarisation_energy)) {
    m$parameters[["polarisation_energy"]] <- polarisation_energy
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
  if (!is.na(heat_capacity)) {
    m$parameters[["heat_capacity"]] <- heat_capacity
  }

  # read a full set of primary factors into shorthand vars
  packr <- m$parameters[["effective_pack_resistance"]]
  pe <- m$parameters[["polarisation_energy"]]
  lp <- m$parameters[["lambda_module_to_ambient"]]
  la <- m$parameters[["lambda_module_AC_to_ambient"]]
  fanp <- m$parameters[["fan_power"]]
  COP <- m$parameters[["COP"]]
  arr <- m$parameters[["arrhenius_resistance"]]
  hc <- m$parameters[["heat_capacity"]]

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

  origmodel <- m
  m$logdata <- m$logdata |> slice(from_idx:to_idx) # restricted range model

  # n.b. the box-constrained optimisation of L-BFGS-B throws an error if any
  # dimension of the box is zero, so we add an epsilon and hope for the best
  bestfit <- optim(
    par = c(packr, pe, lp, la, fanp, COP, arr, hc),
    fn = fm,
    lower = c(if (fixed_parameters[1]) packr else 40,
              if (fixed_parameters[2]) pe else -4,
              if (fixed_parameters[3]) lp else 0,
              if (fixed_parameters[4]) la else 0,
              if (fixed_parameters[5]) fanp else 0,
              if (fixed_parameters[6]) COP else 0.1,
              if (fixed_parameters[7]) arr else -4000,
              if (fixed_parameters[8]) hc else 200),
    upper = c(if (fixed_parameters[1]) packr + 100 else 600,
              if (fixed_parameters[2]) pe + 1 else 0,
              if (fixed_parameters[3]) lp + 0.5 else 15,
              if (fixed_parameters[4]) la + 0.5 else 10,
              if (fixed_parameters[5]) fanp + 200 else 600,
              if (fixed_parameters[6]) COP + 0.5 else 6,
              if (fixed_parameters[7]) arr - 200 else -400,
              if (fixed_parameters[8]) hc + 100 else 1000),
    control = list(maxit = iter_count,
                   ndeps = c(5, 0.2, 0.2, 0.1, 50, 0.5, 200, 10)),
    method = "L-BFGS-B")

  # remove epsilons from the best-fit of fixed parameters
  best_packr = if (fixed_parameters[1]) packr else bestfit$par[1]
  best_pe = if (fixed_parameters[2]) pe else bestfit$par[2]
  best_lp = if (fixed_parameters[3]) lp else bestfit$par[3]
  best_la = if (fixed_parameters[4]) la else bestfit$par[4]
  best_fanp = if (fixed_parameters[5]) fanp else bestfit$par[5]
  best_COP = if (fixed_parameters[6]) COP else bestfit$par[6]
  best_arr = if (fixed_parameters[7]) arr else bestfit$par[7]
  best_hc = if (fixed_parameters[8]) hc else bestfit$par[8]

  # evaluate predict_temp() on the best_fit parameters
  if ((to_idx - from_idx + 1) < length(origmodel$logdata$err_pred)) {
    m <- predict_temp(
      m,
      effective_pack_resistance = best_packr,
      polarisation_energy = best_pe,
      lambda_module_to_ambient = best_lp,
      lambda_module_AC_to_ambient = best_la,
      fan_power = best_fanp,
      COP = best_COP,
      arrhenius_resistance = best_arr,
      heat_capacity = best_hc,
      iter_count = iter_count,
      min_segment_length = min_segment_length,
      trace = trace
    )
    cat("MSE of fit over the specified range:", MSE_of_fit(m), "\n")
  }

  origmodel <- predict_temp(
    origmodel,
    effective_pack_resistance = best_packr,
    polarisation_energy = best_pe,
    lambda_module_to_ambient = best_lp,
    lambda_module_AC_to_ambient = best_la,
    fan_power = best_fanp,
    COP = best_COP,
    arrhenius_resistance = best_arr,
    heat_capacity = best_hc,
    iter_count = iter_count,
    min_segment_length = min_segment_length,
    trace = min(1, trace)
  )

  return(origmodel)
}
