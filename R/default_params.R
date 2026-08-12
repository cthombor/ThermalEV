#' default_params: sets thmodel params to default values, if the parameter
#' list has length 0
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
#' estimate of the pack's OCV as a function of its SOC. My intention is to
#' refine this estimate using observed data -- to gain a more accurate estimate
#' of the effective pack resistance as a function of SOC, and also perhaps to
#' add a second rate constant (as in the Butler-Volmer equation) to model
#' voltage drops (and thus Joule heating) more accurately than is possible with
#' a single-resistor model.  But: 150 mOhm (as derived from the charge-discharge
#' curve at 70% SOC and 0.2C) is currently our default estimate of the effective
#' pack resistance at 25 °C -- at all SOC.
#'
#' The (reversible) enthalpy of polarisation shifts at the beginning and end of
#' a fastcharge are roughly modelled as polarisation_energy * delta_v.  These
#' are shifts seem to cause about in two degrees of cooling at the beginning and
#' two degrees of cooling at the end, in fastcharge sessions on the 50kWh pack.
#' Because this is a reversible heat, its effects while driving are not dramatic
#' over short time intervals -- but they do add up to a significant cooling for
#' each 10V drop in pack OCV (as caused by the SOC drop).
#'
#' @param m a thmodel
#'
#' @returns a thmodel with all required params set
#' @export
#'
#' @examples default_params(new_thmodel())
default_params <- function(m) {
  if (length(m$parameters) == 0) {
    m$parameters <- list(effective_pack_resistance =
                           ifelse(m$capacity == 24, 300, 150),
                         polarisation_energy =
                           ifelse(m$capacity == 24, -1, -2),
                         lambda_module_to_ambient =
                           ifelse(m$capacity == 24, 10, 6),
                         lambda_module_AC_to_ambient =
                           ifelse(m$model == "e-NV200", 1.33, 10),
                         fan_power =
                           ifelse(m$model == "e-NV200", 300, 0),
                         COP = ifelse(m$model == "e-NV200", 3.0, 0),
                         arrhenius_resistance = -3500,
                         heat_capacity = 300,
                         OCV_tbl = tibble(
                           SOC = c(
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
    )
  }
  m$modified.last.time <- now()
  return(m)
}
