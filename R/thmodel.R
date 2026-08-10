# thmodel: S3 class constructors and helpers. sets defaults for params.

#' new_thmodel.  A lean-and-mean constructor, defining only the names and
#' classes of the required fields.
#'
#' I'm relying on Kubal (2022, https://doi.org/10.1016/j.jpowsour.2022.231864)
#' for an estimation of the effective activation energy in the temperature
#' dependence (as estimated by Arrhenius' equation) of the effective resistance
#' of the NCM811 (or perhaps NCM632) cells in the Nissan packs under test.
#' An activation energy in the range from -27 to -31 kJ/mol corresponds to a
#' slope in the range [-3250, -3730] of the log-resistance of the pack (for
#' purposes of a Joule-heating estimation) as a function of temperature.  I
#' have validated -3500 as a plausible fit to the temperature-dependence of
#' the (inaccurately-predicted) effective resistance of the pack, and I don't
#' have anywhere near enough data to form a more accurate estimate of the
#' arrhenius_resistance parameter in my model of the pack's thermal behaviour.
#'
#' @returns thmodel, a named list
#' @export
#'
#' @examples
#' m <- new_thmodel()
new_thmodel <- function() {
  m <-
    list(
      name = "", # user-friendly name, used for titles of plots
      model = "", # either "LEAF" or "e-NV200"
      capacity = NULL, # nominal capacity, in kWh
      filnm = "", # provenance (a csv filnm)
      fildir = "", # provenance (dir relative to a homedir), no trailing "/"
      created.time = now(), # named untidily, as in xts
      modified.last.time = now(),
      parameters = list(),
      # if length(m$parameters) > 0, m$logdata must have the predicted temps.
      fit = list(),
      # if length(m$fit) > 0, m$fit must be the result of an nlm(),
      # and the parameters must be the best-fit estimates found by nlm()
      logdata = tibble(),
      # if logdata has any rows, then this must be a log from
      # LeafSpy as munged by munge_logfile, with additional columns
      # from thmodel.predict_temp() on the parameters
      ocvdata = tibble()
      # if ocvdata has any rows, then this must be an estimate of the OCV
      # at 25 °C, as a function of SOC.
    )
  m <- structure(m, class = "thmodel")
  return(m)
}

#' setter for the name field of thmodel
#'
#' @param m a thmodel
#' @param nm a friendly and informative name for this thmodel
#'
#' @returns the modified thmodel
#' @export
#'
#' @examples
#' m <- set_name_thmodel(predict_temp(), "26Jan2026, 50kWh, no AC")
# todo: dispatch as S3 method through set_name() or set()
set_name_thmodel <- function(m, nm) {
  m$name <- nm
  m$modified.last.time <- now()
  return(m)
}
