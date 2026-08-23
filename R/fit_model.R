#' fit_model: uses optim() to find a best-fit, with MSE criterion
#'
#' @param m a thmodel
#' @param arrhenius_resistance in K
#' @param heat_capacity in kJ/K
#' @param polarisation_energy in kJ/V
#' @param lambda_module_to_ambient in hours
#' @param lambda_module_to_ambient in hours
#' @param fan_power in W
#' @param COP coefficient of heatpump performance, dimensionless
#' @param effective_pack_resistance in mOhms
#' @param packr85 in mOhms
#' @param iter_count controls convergence on predicted temps
#' @param min_segment_length shorter sequences of samples are ignored
#' @param fixed_parameters length-9 Boolean vector, reduces dimension of opt
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
    polarisation_energy = NA,
    lambda_module_to_ambient = NA,
    lambda_module_AC_to_ambient = NA,
    fan_power = NA,
    COP = NA,
    effective_pack_resistance = NA,
    packr85 = NA,
    iter_count = 4,
    min_segment_length = 20,
    # packr and r85 are optimised by fit_r_to_ocv()
    fixed_parameters = c(F, F, F, F, F, F, F, T, T),
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
  fm <- function(x = c(arr, hc, pe, lp, la, fanp, COP, packr, r85)) {
    m <- predict_temp(
      m,
      arrhenius_resistance = x[1],
      heat_capacity = x[2],
      polarisation_energy = x[3],
      lambda_module_to_ambient = x[4],
      lambda_module_AC_to_ambient = x[5],
      fan_power = x[6],
      COP = x[7],
      effective_pack_resistance = x[8],
      packr85 = x[9],
      trace = trace
    )
    return(MSE_of_fit(m))
  }

  if (is.null(m)) m <- munge_logfile()  # use our default logfile

  stopifnot((length(m$parameters) == 0) || (length(m$parameters) == 10))
  if (length(m$parameters) == 0) {
    m <- default_params(m)
  }

  # param values specified in the method call have precedence. Side effect:
  # if m$parameters is malformed, throw a "subscript out of bounds" error
  if (!is.na(effective_pack_resistance)) {
    m$parameters[["effective_pack_resistance"]] <- effective_pack_resistance
  }
  if (!is.na(packr85)) {
    m$parameters[["packr85"]] <- packr85
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
  arr <- m$parameters[["arrhenius_resistance"]]
  hc <- m$parameters[["heat_capacity"]]
  pe <- m$parameters[["polarisation_energy"]]
  lp <- m$parameters[["lambda_module_to_ambient"]]
  la <- m$parameters[["lambda_module_AC_to_ambient"]]
  fanp <- m$parameters[["fan_power"]]
  COP <- m$parameters[["COP"]]
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

  origmodel <- m
  m$logdata <- m$logdata |> slice(from_idx:to_idx) # restricted range model

  # n.b. the box-constrained optimisation of L-BFGS-B throws an error if any
  # dimension of the box is zero, so we add an epsilon and hope for the best
  bestfit <- optim(
    par = c(arr, hc, pe, lp, la, fanp, COP, packr, r85),
    fn = fm,
    lower = c(if (fixed_parameters[1]) arr else -4000,
              if (fixed_parameters[2]) hc else 200,
              if (fixed_parameters[3]) pe else -16,
              if (fixed_parameters[4]) lp else 0,
              if (fixed_parameters[5]) la else 0,
              if (fixed_parameters[6]) fanp else 0,
              if (fixed_parameters[7]) COP else 0.1,
              if (fixed_parameters[8]) packr else 40,
              if (fixed_parameters[9]) r85 else 40),
    upper = c(if (fixed_parameters[1]) arr - 20 else -400,
              if (fixed_parameters[2]) hc + 2 else 1000,
              if (fixed_parameters[3]) pe + 0.2 else 64,
              if (fixed_parameters[4]) lp + 0.2 else 15,
              if (fixed_parameters[5]) la + 0.2 else 10,
              if (fixed_parameters[6]) fanp + 20 else 600,
              if (fixed_parameters[7]) COP + 0.2 else 6,
              if (fixed_parameters[8]) packr + 0.2 else 600,
              if (fixed_parameters[9]) r85 + 0.2 else 600),
    control = list(maxit = iter_count,
                   ndeps = c(10, 1, 0.1, 0.1, 0.1, 10, 0.1, 0.1, 0.1)),
    method = "L-BFGS-B")

  # remove epsilons from the best-fit of fixed parameters
  if (FALSE) {
    best_arr = if (fixed_parameters[1]) arr else bestfit$par[1]
    best_hc = if (fixed_parameters[2]) hc else bestfit$par[2]
    best_pe = if (fixed_parameters[3]) pe else bestfit$par[3]
    best_lp = if (fixed_parameters[4]) lp else bestfit$par[4]
    best_la = if (fixed_parameters[5]) la else bestfit$par[5]
    best_fanp = if (fixed_parameters[6]) fanp else bestfit$par[6]
    best_COP = if (fixed_parameters[7]) COP else bestfit$par[7]
    best_packr = if (fixed_parameters[8]) packr else bestfit$par[8]
    best_r85 = if (fixed_parameters[9]) r85 else bestfit$par[9]
  } else {
    best_arr = bestfit$par[1]
    best_hc = bestfit$par[2]
    best_pe = bestfit$par[3]
    best_lp = bestfit$par[4]
    best_la = bestfit$par[5]
    best_fanp = bestfit$par[6]
    best_COP = bestfit$par[7]
    best_packr = bestfit$par[8]
    best_r85 = bestfit$par[9]

  }

  # evaluate predict_temp(m) on the best_fit parameters
  m <- predict_temp(
    m,
    arrhenius_resistance = best_arr,
    heat_capacity = best_hc,
    polarisation_energy = best_pe,
    lambda_module_to_ambient = best_lp,
    lambda_module_AC_to_ambient = best_la,
    fan_power = best_fanp,
    COP = best_COP,
    effective_pack_resistance = best_packr,
    packr85 = best_r85,
    iter_count = iter_count,
    min_segment_length = min_segment_length,
    trace = trace
  )

  if ((to_idx - from_idx + 1) < length(origmodel$logdata$err_pred)) {
    cat("MSE of fit over the specified range:", round(MSE_of_fit(m), 3), "\n")
    # evaluate predict_temp() on the best_fit parameters, full model
    m <- predict_temp(
      orig_model,
      arrhenius_resistance = best_arr,
      heat_capacity = best_hc,
      polarisation_energy = best_pe,
      lambda_module_to_ambient = best_lp,
      lambda_module_AC_to_ambient = best_la,
      fan_power = best_fanp,
      COP = best_COP,
      effective_pack_resistance = best_packr,
      packr85 = best_r85,
      iter_count = iter_count,
      min_segment_length = min_segment_length,
      trace = trace
    )
  }
  cat("MSE of fit over the full model:", round(MSE_of_fit(m) , 3), "\n")

  return(m)
}
