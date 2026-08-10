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
#' I note that there are much more accurate models, but these require more
#' parameters.  The Butler-Volmer equation (and its extensions) cover a much
#' wider range of operating conditions.  See
#' https://en.wikipedia.org/wiki/Butler%E2%80%93Volmer_equation and
#' https://www.sciencedirect.com/science/article/pii/S1452398125002512. But...
#' the C rates of the 50 kWh pack are usually so low that (it seems) it's the
#' lithiation of the graphite which accounts for the lion's share of its Joule
#' heating in my logfiles.  The enthalpy of polarisation shifts during a
#' fastcharge is (I suspect) more important to the inaccuracy of my model than
#' the de-solvation of the ions in the electrolyte, which in turn may be more
#' important than the cathodic half-reaction in an NMC811 cell.
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
                         heat_capacity = 300
    )
  }
  m$modified.last.time <- now()
  return(m)
}
