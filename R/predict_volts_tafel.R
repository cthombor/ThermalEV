#' Predicts pack_volts from an SOC lookup in ocv_tbl, conditioned by pack_amps,
#' hx, and pack_temperature.  Parameters include a polarisation "resistance" (in
#' a Tafel model).
#'
#' @param tm a thmodel
#' @param polarisation_irr in mOhms, baseline polarisation response
#' @param tafel_slope large-C polarisation response
#' @param lambda_polarisation in s, decay rate of polarisation
#' @param arrhenius_tafel in K, Arrhenius coefficient for tafel_slope
#' @param soc_offset_corr offset of actual SOC from LeafSpy-reported SOC
#' @param soc_slope_corr slope-correction for LeafSpy-reported SOC
#' @param trace 0 for silent, 1 for minimal, 2 for verbose, 3 with a plot
#'
#' @returns a thmodel with volt-prediction columns in its logdata
#' @export
#'
#' @examples
#' m <- predict_volts_tafel(eNV50kWh)

predict_volts_tafel <- function(tm = NULL,
                        polarisation_irr = 60,
                        tafel_slope = 0.5,
                        lambda_polarisation = 120,
                        arrhenius_tafel = -3500,
                        soc_offset_corr = 0.35,
                        soc_slope_corr = 0.8,
                        ocv_tbl = NULL,
                        trace = 2) {
  stopifnot(!is.null(tm))
  stopifnot(class(tm) == "thmodel")

  # parameters to predict_volts_tafel have precedence over tm$parameters,
  # and their values (if !is.na()) are written into tm$parameters
  if (!is.na(polarisation_irr))
    tm$parameters$polarisation_irr <- polarisation_irr
  if (!is.na(tafel_slope))
    tm$parameters$tafel_slope <- tafel_slope
  if (!is.na(lambda_polarisation))
    tm$parameters$lambda_polarisation <- lambda_polarisation
  if (!is.na(arrhenius_tafel))
    tm$parameters$arrhenius_tafel <- arrhenius_tafel
  if (!is.na(soc_offset_corr))
    tm$parameters$soc_offset_corr <- soc_offset_corr
  if (!is.na(arrhenius_tafel))
    tm$parameters$soc_slope_corr <- soc_slope_corr
  if (!is.null(ocv_tbl))
    tm$parameters$ocv_tbl <- ocv_tbl

  # read a full set of voltage-prediction parameters
  polarisation_irr <- tm$parameters$polarisation_irr
  tafel_slope <- tm$parameters$tafel_slope
  lambda_polarisation <- tm$parameters$lambda_polarisation
  arrhenius_tafel <- tm$parameters$arrhenius_tafel
  ocv_tbl <- tm$parameters$ocv_tbl
  soc_offset_corr <- tm$parameters$soc_offset_corr
  soc_slope_corr <- tm$parameters$soc_slope_corr

  if (trace > 0) {
    cat(paste0("predict_volts_tafel: ",
               "pi = ", round(polarisation_irr, 3), "\u2009mΩ, ",
               "ts = ", round(tafel_slope, 3), ", ",
               "λp = ", round(lambda_polarisation, 1), "\u2009s, ",
               "at = ", round(arrhenius_tafel, 0),
               "so = ", soc_offset_corr, ", ",
               "ss = ", soc_slope_corr, ", ",
               "\nsummary(ocv_tbl):\n"))
    print(summary(ocv_tbl))
  }

  # predict OCV from SOC, for comparison
  f_soc_to_v <- approxfun(ocv_tbl, method = "linear", rule = 2)

  tm$logdata <- tm$logdata |>
    group_by(segnum) |>
    mutate(
      sampling_interval = mean(delta_t, na.rm = T),
      corr_soc = - soc_offset_corr + (soc / 1e6) / soc_slope_corr,
      tafel_slope_adj =
        exp(arrhenius_tafel *
              ((1 / 298.15) - (1 / (pack_avg_temp + 273.15)))),
      # TODO: find a way to estimate the dependence of tafel_slope on hx
      # n.b. we're using the measured pack temps, not the predicted ones,
      # so no iteration is necessary (as in predict_temp())
      EMA_parameter_polarisation =
        min(1.0, sampling_interval / lambda_polarisation),
      # TODO: should this lambda be adjusted for pack_temp?
      est_overvoltage = NA
      ) |>
    ungroup()

  for (i in seq(max(tm$logdata$segnum))) {
    #TODO: tidy this code (after it is debugged and stable)
    mv <- which(tm$logdata$segnum == i)
    if (length(mv) > 0) {
      ia <- tm$logdata$pack_amps[mv] # instantaneous amperage
      iv <- - sign(ia) * exp(tafel_slope * log(abs(ia))) *
        (polarisation_irr / 1000)
      # n.b. resistances are in mOhms.  pack_amps is negative when charging.
      # TODO: use an ohmic (charge-transfer) model at small abs(ia)?
      # TODO: are overvoltages diffusion-limited at large abs(ia)?  See
      # https://doi.org/10.1021/acs.jpcc.9b06820
      lambda <- tm$logdata$EMA_parameter_polarisation[mv]
      pv <- stats::filter(
        iv, # impulse voltage
        filter = c(1 - lambda[[1]]),
        method = "recursive", # exponential decay
        init = iv[[1]]
      )
      tm$logdata$est_overvoltage[mv] <- pv
    }
  }
  tm$logdata <- tm$logdata |>
    mutate(
      pred_pack_volts_from_soc = f_soc_to_v(corr_soc) + est_overvoltage
    )
  #n.b. we're using a soc that's corrected to align with the ocv_tbl

  if (trace > 0) cat("MSE of fit:", round(MSE_of_ocv_fit(tm), 2), "\n")

  if (trace > 2) {
    plot(plot_volts_pred_volts(tm))
  }

  return(tm)

}
