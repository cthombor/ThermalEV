#' Predicts pack_volts from OCV(SOC), pack_amps, hx, pack_temperature, arr
#'
#' @param om an ocv_model
#' @param effective_pack_resistance in mOhms at 298.15K
#' @param arrhenius_resistance in K
#' @param packr85 in mOhms at 298.15K, at SOC = 0.85 (inflection at soc = 0.7)
#' @param trace 0 for silent, 1 for minimal, 2 for verbose
#'
#' @returns a ocv_model, with a pred_pack_volts column in its logdata
#' @export
#'
#' @examples
#' om <- predict_ocv(new_ocv_model("eNV50kWh", list(eNV200ac50kWh)))

predict_ocv <- function(om = NULL,
                        effective_pack_resistance = 67,
                        arrhenius_resistance = -3500,
                        packr85 = 0,
                        trace = 1) {
  stopifnot(!is.null(om))

  if (trace > 0) {
    cat(paste0("predict_ocv: r = ", round(effective_pack_resistance, 5),
               ", a = ", round(arrhenius_resistance, 5),
               ", r85 = ", round(packr85, 3),
               "\n"))
  }

  if (!is.na(effective_pack_resistance))
    om$parameters[["effective_pack_resistance"]] <- effective_pack_resistance
  if (!is.na(arrhenius_resistance))
    om$parameters[["arrhenius_resistance"]] <- arrhenius_resistance
  if (!is.na(packr85))
    om$parameters[["packr85"]] <- packr85

  f_soc_to_v <- splinefun(om$ocv_tbl, method = c("natural"))

  om$logdata <- om$logdata |>
    mutate(
      eff_packr = (effective_pack_resistance +
                     ifelse(soc < 0.7,
                            0,
                            (soc - 0.7) / 0.15 *
                              (packr85 - effective_pack_resistance))) *
        exp(arrhenius_resistance *
              (1 / 298.15 - 1 / (pack_avg_temp + 273.15))) / (hx / 100),
      pred_pack_volts = f_soc_to_v(soc) - pack_amps * eff_packr / 1000
    )

  return(om)

}
