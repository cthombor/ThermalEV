#' add_dvdj_to_ocv_tbl: augment an ocv_tbl with DVDJ and DVDC columns
#'
#' @param ocvtbl an ocv_tbl with columns SOC and OCV
#' @param capacity_kWh nominal capacity of the pack in kWh at SOH = 1.0
#' @param capacity_Ah nominal capacity of the pack in amp-hours at SOH = 1.0
#' @param soh the SOH for which this ocv_tbl is valid. Default is 1.0.
#'
#' @returns an ocv_tbl with a DVDJ column (in V/J) and a DVDC column (in V/As)
#' @export
#'
#' @examples
#' ot50 <- add_dvdj_to_ocv_tbl(ocvtbl) # 50kWh pack at 100% SOH
#' ot24 <- add_dvdj_to_ocv_tbl(ot24, 24, 66, 0.8) # 24kWh pack at 80% SOH

add_dvdj_to_ocv_tbl <- function(ocvtbl,
                             capacity_kWh = 50,
                             capacity_Ah = 137,
                             soh = 1.0)
{
  stopifnot(("SOC" %in% names(ocvtbl)) && ("OCV" %in% names(ocvtbl)))
  capacity_J = 3.6e6 * capacity_kWh * soh # pack capacity in Joules
  capacity_As = 3.6e3 * capacity_Ah * soh # pack capacity in Coulombs
  ocvtbl <- ocvtbl |>
    arrange(SOC) |>
    mutate(
      DVDJ =
        (lead(OCV, default = last(OCV)) - lag(OCV, default = first(OCV))) /
        (capacity_J *
           (lead(SOC, default = last(SOC)) - lag(SOC, default = first(SOC)))),
      # dV is estimated from a 3-point (symmetric) difference, except at
      # extremal SOC where a 2-point difference is used
      # dJ is assumed to scale with SOC.
      # TODO: estimate dJ from a constant-current charge-discharge curve.
      DVDC =
        (lead(OCV, default = last(OCV)) - lag(OCV, default = first(OCV))) /
        (capacity_As *
           (lead(SOC, default = last(SOC)) - lag(SOC, default = first(SOC)))
        )
      # an SOC which scales with Ah, rather than with kWh, may well be the
      # case for the Nissan soc, which apparently scales with gids at 80 Wh/gid.
      # TODO: estimate dvDC from a kWh-based SOC, and/or adjust predict_temp()
      # so that it counts Ws rather than Joules in its timeseries adjustments
      # to est_ocv_from_pack_behaviour.
    )
  ocvtbl
}
