#' default_params: sets thmodel params to default values
#'
#' I'm relying on Kubal (2022, https://doi.org/10.1016/j.jpowsour.2022.231864)
#' for an estimation of the effective activation energy in the temperature
#' dependence (as estimated by Arrhenius' equation) of the effective resistance
#' of the NCM811 (or perhaps NCM632) cells in the Nissan packs under test.
#' An activation energy in the range from -27 to -31 kJ/mol corresponds to a
#' slope in the range [-3250, -3730] of the log-resistance of the pack (for
#' purposes of a Joule-heating estimation) as a function of temperature.  I
#' have validated -3500 as a plausible fit to the temperature-dependence of
#' the (inaccurately-predicted) effective resistance of the pack.  I don't
#' have anywhere near enough data to form a more accurate estimate of the
#' arrhenius_resistance parameter in my model of the pack's thermal behaviour.
#'
#' I'm relying on fragments of a manufacturer's spec sheet posted online by a
#' retailer of the CALB L300N137B 3.7 volt 3.7V 137ah lithium nmc battery cell,
#' for a charge-discharge curve at 0.2C which I interpolated to derive a first
#' estimate of the pack's OCV as a function of its SOC. Experimentation has
#' revealed that the LeafSpy-reported "soc" has a zero-point that's different
#' to the lowest operating voltage for the cells.  I'm now computing a cSOC
#' by adding an offset (of approximately 0.14) to a scaled LeafSpy
#' lSOC == soc/1e6, so that the full range of cell voltages is covered by
#' cSOC in the range 0 to 1.  To add to the confusion, the dashboard-reported
#' state of charge (which I'd call dSOC) is different to the lSOC -- perhaps
#' to discourage drivers from expecting to get any range in the bottom few
#' percentage points of lSOC.
#'
#' TODO: look for evidence that the anodic response (discharging) is different
#' enough to the cathodic response (charging) that it'd be worthwhile adding
#' additional parameters to more accurately characterise the overvoltage eta.
#'
#' Polarisation shifts near the cells' electrodes are entropic rearrangements of
#' the ions which cause some reversible heating and cooling whenever the pack's
#' voltage shifts.  Additionally, the graphite anode must lithiate and
#' de-lithiate as the cell is charged and discharged.  The per-Volt entropic
#' changes at the anode depend on the "gallery" of lithium-to-carbon association
#' (which varies from 0:1 to 6:1 depending on the SOC).  Our parameterisation
#' has the dimensions of mV/J. Because the entropic energy is reversible and not
#' tremendously large, it need not be very accurately located within the
#' time-series.  We therefore use the delta in pack_voltage as our estimate of
#' an OCV shift.  In our current version (v0.26), we use pack_amps to estimate
#' the current flow through the cathode, with no correction for the fraction of
#' pack_amps which our simulation attributes to polarisation currents.  If we
#' are successful in deriving an accurate estimate of the pack's OCV, this would
#' allow a more accurate estimation of the charging current -- and this would
#' allow a more accurate estimation of the magnitude and timing of the ebbs and
#' flows of entropy within the cathode and the electrolyte.
#'
#' As a first estimate of heat_capacity: The 96 cells in my aftermarket 50kWh
#' pack weigh 2.13 kg apiece.  If the solvent in their electrolyte is DME
#' (1,2-dimethoxyethane), it has a heat capacity of 192 J/mol.K, i.e. about 2.1
#' J/gK.  If the solvent is ethylene carbonate, then it would have a lower heat
#' capacity: 134 J/mol.K = 1.6 J/gK. The primary salt in the electrolyte might
#' be lithium hexafluorophosphate (152 J/mol.K, 151 g/mol, 1.0 J/gK).  Its
#' presence increases the density of the electrolyte, but its lower heat content
#' would decrease the per-gram heat capacity of a 1:1 mix of DME and EC to 1.4
#' J/gK, if it is in a 1M solution. The casings of the prismatic cells may be
#' polyethylene (at 2.0 J/gK). The module casings seem to be steel (at 0.5
#' J/gK).  The shell of the pack is aluminium (at 0.9 J/gK). As a round number,
#' the total weight of the pack is 220 kg.  Its heat capacity might thus be 300
#' to 400 kJ/K.  An alternative estimate from the literature: a 60 Ah NMC811
#' pouch cell was estimated as having a specific heat capacity of 1.1 J/gK
#' (see https://www.sciencedirect.com/science/article/pii/S1452398125002512),
#' suggesting that the heat capacity of the VIVNE 50kWh pack might be
#' 250 kJ/K.  Our initial estimate of heat_capacity is thus 300 kJ/K for the
#' 50kWh pack.  The 24kWh pack has approximately the same weight, so we use the
#' same initial estimate of 300 kJ/K for its heat_capacity.  Our temperature
#' and voltage models are imprecise, and the pack resistance parameters are
#' almost entirely collinear with the pack's heat capacity (with the exception
#' of the reversible heat of polarisation); but our best-fits suggest a
#' heat capacity of approximately 250 kJ/K.
#'
#' 1 GID = 80 Wh, as confirmed experimentally in https://cthombor.
#' wpcomstaging.com/50kwh-upgrade-to-my-e-nv200/50kwh-upgrade-to-my-
#' 24kwh-2014-nissan-e-nv200-part-3-estimation-of-usable-kwh/.
#' The dashboard-reported SOC is adjusted to have a 0-point when the
#' pack has about 10% of its nominal capacity.  The LeafSpy-reported
#' SOC has no allowance for the "reserve"; and the vehicle will
#' show a "turtle" on the dashboard when the LeafSpy-reported SOC
#' is approximately 10% of its maximum value.
#' A cursory examination of plot_gids(eNV50kWh) reveals that the
#' firmware for the BMS in this 50 kWh pack produces an estimate
#' for SOC that is often quite inaccurate when the pack's SOC is between
#' 1/3 and 1/2 of its nominal capacity.  The BMS in Alastair's 50 kWh
#' pack might, or might not, have a similar issue given that there
#' is such little diversity in the operating conditions of
#' eNVa.50kWh.2025.  The OEM BMS in my 24kWh e-NV200 shows an excellent
#' fit to SOC = 9.9 + 0.309 * GID / SOH, over the quite varied operating
#' conditions of eNV24kWh. The BMS in my 24kWh Leaf shows an excellent
#' fit to SOC = 11.2 + 0.299 GID / SOH over the operating
#' conditions of Leaf24kWh_2019 (which had no SOC < 45).  These fits motivate
#' my default value of 10 for gids_reserve.
#'
#' @param m a thmodel
#' @param ot an ocv_tbl, estimated from voltage behaviour (est_ocv())
#'
#' @returns a thmodel with all required params set
#' @export
#'
#' @examples default_params(new_thmodel())
default_params <- function(m,
                           ot = NULL) {
  stopifnot(!is.na(m$capacity))
  stopifnot(!is.null(m$model))
  stopifnot(!is.null(ot) ||
              !(tibble::is_tibble(ot) && names(ot) == c("SOC", "OCV")))

  if (is.null(ot)) {
    ot = tibble(
      SOC = c( # from cell spec sheet. N.b. Nissan's soc is quite different
        0.000,
        0.005,
        0.012,
        0.060,
        0.117,
        0.177,
        0.237,
        0.294,
        0.354,
        0.409,
        0.471,
        0.534,
        0.589,
        0.648,
        0.708,
        0.766,
        0.823,
        0.883,
        0.943,
        0.983,
        1.000
      ),
      OCV = c(
        295.2,
        306.0,
        315.4,
        327.5,
        334.2,
        338.9,
        342.0,
        344.6,
        347.1,
        349.6,
        352.0,
        357.2,
        361.5,
        367.4,
        372.5,
        378.1,
        384.1,
        390.9,
        395.9,
        399.7,
        403.2
      )
    )
  }

  m$parameters <- list(arrhenius_resistance =
                         ifelse(m$capacity == 24, -3500, -3500),
                       heat_capacity =
                         ifelse(m$capacity == 24, 210, 234),
                       entropic_heat =
                         ifelse(m$capacity == 24, -6, -6),
                       lambda_module_to_ambient =
                         ifelse(m$capacity == 24, 7.5, 7.5),
                       lambda_module_AC_to_ambient =
                         ifelse(m$model == "e-NV200", 1.14, 1.14),
                       fan_power =
                         ifelse(m$model == "e-NV200", 300, 0),
                       COP = ifelse(m$model == "e-NV200", 2.9, 0),
                       effective_pack_resistance =
                         ifelse(m$capacity == 24, 180, 60),
                       polarisation_Rp =
                         ifelse(m$capacity == 24, 20, 20),
                       polarisation_R2 =
                         ifelse(m$capacity == 24, 20, 20),
                       arrhenius_R2 =
                         ifelse(m$capacity == 24, -3500, -3500),
                       lambda1_polarisation =
                         ifelse(m$capacity == 24, 120, 120),
                       lambda2_polarisation =
                         ifelse(m$capacity == 24, 20, 20),
                       gids_reserve =
                         ifelse(m$capacity == 24, 40, 62.5),
                       ocv_tbl =
                         add_dvdj_to_ocv_tbl(
                           ot,
                           capacity_kWh = m$capacity,
                           capacity_Ah = 137 * m$capacity / 50,
                           soh = 1.0)
  )
  m$modified.last.time <- lubridate::now()
  return(m)
}
