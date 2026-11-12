#' fit_model_elec: uses optim() to find a best-fit of seven params in the,
#' electrical model, with a MSE criterion
#'
#' @param m a thmodel
#' @param effective_pack_resistance in mOhms, the sum of R1 and Rp
#' @param polarisation_Rp in mOhms
#' @param polarisation_R2 in mOhms
#' @param lambda1_polarisation in s, for the R1 - (Rp // C1) branch of the EC
#' @param lambda2_polarisation in s, for the R2 - C2 branch of the EC
#' @param entropic_heat in kJ/V
#' @param ocv_relax, dimensionless, affects OCV estimation in predict_temp()
#' @param gids reserve in gids, not optimised
#' @param heat_capacity in kJ/K, not optimised
#' @param lambda_module_to_ambient in hours, not optimised
#' @param lambda_module_AC_to_ambient in hours, not optimised
#' @param fan_power in W, not optimised
#' @param COP coefficient of heatpump performance, dimensionless, not optimised
#' @param arrhenius_resistance in K, not optimised
#' @param arrhenius_R2 in K, not optimised
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
#' m <- fit_model_elec(eNV50kWh)
fit_model_elec <- function(
    m = NULL,
    effective_pack_resistance = NA,
    polarisation_Rp = NA,
    polarisation_R2 = NA,
    lambda1_polarisation = NA,
    lambda2_polarisation = NA,
    entropic_heat = NA,
    ocv_relax = 1,
    gids_reserve = NA,
    heat_capacity = NA,
    lambda_module_to_ambient = NA,
    lambda_module_AC_to_ambient = NA,
    fan_power = NA,
    COP = NA,
    arrhenius_resistance = NA,
    arrhenius_R2 = NA,
    iter_count = 4,
    min_segment_length = 20,
    fixed_parameters = c(F, F, F, F, F, F),
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
#'    effective_pack_resistance = NA,

  fm <- function(x = c(reff, rp, r2, lp1, lp2, eh)) {
    m <- predict_temp(
      m,
      effective_pack_resistance = x[1],
      polarisation_Rp = x[2],
      polarisation_R2 = x[3],
      lambda1_polarisation = x[4],
      lambda2_polarisation = x[5],
      entropic_heat = x[6],
      gids_reserve = gr,
      heat_capacity = hc,
      lambda_module_to_ambient = lamb,
      lambda_module_AC_to_ambient = lpass,
      fan_power = fanp,
      COP = COP,
      arrhenius_resistance = arr,
      arrhenius_R2 = arr2,
      iter_count = iter_count,
      min_segment_length = min_segment_length,
      trace = trace
    )
    return(MSE_of_fit(m))
  }

  if (is.null(m)) m <- munge_logfile()  # use our default logfile

  if (length(m$parameters) == 0) {
    m <- default_params(m)
  }

  stopifnot(length(fixed_parameters) == 6)

  # param values specified in the method call have precedence.
  if (!is.na(effective_pack_resistance))
    m$parameters$effective_pack_resistance <- effective_pack_resistance
  if (!is.na(polarisation_Rp))
    m$parameters$polarisation_Rp <- polarisation_Rp
  if (!is.na(polarisation_R2))
    m$parameters$polarisation_R2 <- polarisation_R2
  if (!is.na(lambda1_polarisation))
    m$parameters$lambda1_polarisation <- lambda1_polarisation
  if (!is.na(lambda2_polarisation))
      m$parameters$lambda2_polarisation <- lambda2_polarisation
  if (!is.na(entropic_heat))
    m$parameters$entropic_heat <- entropic_heat
  if (!is.na(gids_reserve))
    m$parameters$gids_reserve <- gids_reserve
  if (!is.na(heat_capacity))
    m$parameters$heat_capacity <- heat_capacity
  if (!is.na(lambda_module_to_ambient))
    m$parameters$lambda_module_to_ambient <- lambda_module_to_ambient
  if (!is.na(lambda_module_AC_to_ambient))
    m$parameters$lambda_module_AC_to_ambient <- lambda_module_AC_to_ambient
  if (!is.na(fan_power))
    m$parameters$fan_power <- fan_power
  if (!is.na(COP))
    m$parameters$COP <- COP
  if (!is.na(arrhenius_resistance))
    m$parameters$arrhenius_resistance <- arrhenius_resistance
  if (!is.na(arrhenius_R2))
    m$parameters$arrhenius_R2 <- arrhenius_R2

  # read a full set of primary factors into shorthand vars
  packr <- m$parameters$effective_pack_resistance
  rp <- m$parameters$polarisation_Rp
  r2 <- m$parameters$polarisation_R2
  lp1 <- m$parameters$lambda1_polarisation
  lp2 <- m$parameters$lambda2_polarisation
  eh <- m$parameters$entropic_heat
  gr <- m$parameters$gids_reserve
  hc <- m$parameters$heat_capacity
  lamb <- m$parameters$lambda_module_to_ambient
  lpass <- m$parameters$lambda_module_AC_to_ambient
  fanp <- m$parameters$fan_power
  COP <- m$parameters$COP
  arr <- m$parameters$arrhenius_resistance
  arr2 <- m$parameters$arrhenius_R2

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
    par = c(packr, rp, r2, lp1, lp2, eh),
    fn = fm,
    lower = c(if (fixed_parameters[1]) packr else 40,
              if (fixed_parameters[2]) rp else 5,
              if (fixed_parameters[3]) r2 else 10,
              if (fixed_parameters[4]) lp1 else 15,
              if (fixed_parameters[5]) lp2 else 15,
              if (fixed_parameters[6]) eh else -100),
    upper = c(if (fixed_parameters[1]) packr + 50 else 200,
              if (fixed_parameters[2]) rp + 50 else 190,
              if (fixed_parameters[3]) r2 + 50 else 190,
              if (fixed_parameters[4]) lp1 + 60 else 240,
              if (fixed_parameters[5]) lp2 + 30 else 60,
              if (fixed_parameters[6]) eh + 30 else 100),
    control = list(maxit = iter_count,
                   ndeps = c(1, 1, 10, 10, 10, 10)),
    method = "L-BFGS-B")

  # remove epsilons from the best-fit of fixed parameters
  best_packr = ifelse (fixed_parameters[1], packr, bestfit$par[1])
  best_rp = ifelse (fixed_parameters[2], rp, bestfit$par[2])
  best_r2 = ifelse (fixed_parameters[3], r2, bestfit$par[3])
  best_lp1 = ifelse (fixed_parameters[4], lp1, bestfit$par[4])
  best_lp2 = ifelse (fixed_parameters[5], lp2, bestfit$par[5])
  best_eh = ifelse (fixed_parameters[6], eh, bestfit$par[6])

  # evaluate predict_temp(m) on the best_fit parameters
  # n.b. the fit will be degraded by any epsilon-shifts in sensitive params
  m <- predict_temp(
    m,
    effective_pack_resistance = best_packr,
    polarisation_Rp = best_rp,
    polarisation_R2 = best_r2,
    lambda1_polarisation = best_lp1,
    lambda2_polarisation = best_lp2,
    entropic_heat = best_eh,
    gids_reserve = gr,
    heat_capacity = hc,
    lambda_module_to_ambient = lamb,
    lambda_module_AC_to_ambient = lpass,
    fan_power = fanp,
    COP = COP,
    arrhenius_resistance = arr,
    arrhenius_R2 = arr2,
    iter_count = iter_count,
    min_segment_length = min_segment_length,
    trace = ifelse(trace == 0, 0, trace + 1)
  )

  if ((to_idx - from_idx + 1) < length(orig_model$logdata$err_pred)) {
    cat("MSE of fit over the specified range:", round(MSE_of_fit(m), 3), "\n")
    # evaluate predict_temp() on the best_fit parameters, full model
    m <- predict_temp(
      orig_model,
      effective_pack_resistance = best_packr,
      polarisation_Rp = best_rp,
      polarisation_R2 = best_r2,
      lambda1_polarisation = best_lp1,
      lambda2_polarisation = best_lp2,
      entropic_heat = best_eh,
      gids_reserve = gr,
      heat_capacity = hc,
      lambda_module_to_ambient = lamb,
      lambda_module_AC_to_ambient = lpass,
      fan_power = fanp,
      COP = COP,
      arrhenius_resistance = arr,
      arrhenius_R2 = arr2,
      iter_count = iter_count,
      min_segment_length = min_segment_length,
      trace = ifelse(trace == 0, 0, trace + 1)
    )
  }
  cat("MSE of fit over the full model:", round(MSE_of_fit(m) , 3), "\n")

  return(m)
}
