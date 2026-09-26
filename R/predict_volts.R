#' Predicts pack_volts from an SOC lookup in ocv_tbl, conditioned by pack_amps,
#' hx, and pack_temperature.  Parameters include a polarisation "resistance" (in
#' a Tafel model).
#'
#' @param om an ocv_model
#' @param polarisation_irr in mOhms, polarisation response to small Δ pack_amps
#' @param tafel_slope in volts/(amps⋅(ln amps)), high-C response to Δ pack_amps
#' @param lambda_polarisation in s, decay of pack_volts response to Δ pack_amps
#' @param arrhenius_tafel in K, Arrhenius coefficient for tafel_slope
#' @param trace 0 for silent, 1 for minimal, 2 for verbose
#'
#' @returns an ocv_model with prediction columns in its logdata
#' @export
#'
#' @examples
#' om <- predict_volts(om_eNV50kWh)

predict_volts <- function(om = NULL,
                        effective_pack_resistance = NA,
                        packr85 = NA,
                        polarisation_irr = NA,
                        arrhenius_resistance = NA,
                        trace = 1) {
  stopifnot(!is.null(om))
  stopifnot(class(om) == "ocv_model")

  # parameters to predict_volts have precedence over om$parameters
  if (!is.na(effective_pack_resistance))
    om$parameters[["effective_pack_resistance"]] <- effective_pack_resistance
  if (!is.na(arrhenius_resistance))
    om$parameters[["arrhenius_resistance"]] <- arrhenius_resistance
  if (!is.na(packr85))
    om$parameters[["packr85"]] <- packr85
  effective_pack_resistance <- om$parameters[["effective_pack_resistance"]]
  arrhenius_resistance <- om$parameters[["arrhenius_resistance"]]
  packr85 <- om$parameters[["packr85"]]

  if (trace > 0) {
    cat(paste0("predict_volts: r = ", round(effective_pack_resistance, 3),
               ", r85 = ", round(packr85, 3),
               ", a = ", round(arrhenius_resistance, 1),
               "; "))
  }

  f_soc_to_v <- approxfun(om$ocv_tbl, method = "linear", rule = 2)
  sloper <- (packr85 - effective_pack_resistance) / 0.15

  om$logdata <- om$logdata |>
    mutate(
      # constant resistance for soc in (0, 0.7); linear in soc for soc in (0.7,
      # 1.0) with value packr85 at soc = 0.85.
      eff_packr =
        if_else(
          soc <= 0.70,
          effective_pack_resistance,
          effective_pack_resistance + sloper * (soc - 0.70)
        ) *
        exp(arrhenius_resistance *
              ((1 / 298.15) - (1 / (pack_avg_temp + 273.15)))) /
        (hx / 100),
      # n.b. resistances are in mOhms
      pred_pack_volts = f_soc_to_v(soc) - pack_amps * eff_packr / 1000
    )

  if (trace > 0) cat("MSE of fit:", round(MSE_of_ocv_fit(om), 2), "\n")

  return(om)

}
