# thmodel: S3 class constructors and helpers

#' new_thmodel.  A lean-and-mean constructor, defining only the names and
#' classes of the required fields.
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
      logdata = tibble()
      # if logdata has any rows, then this must be a log from
      # LeafSpy as munged by munge_logfile, with additional columns
      # from thmodel.predict_temp() on the parameters
    )
  m <- structure(m, class = "thmodel")
  return(m)
}

#' default_params: sets thmodel params to default values, if the parameter
#' list has length 0
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
                           ifelse(m$capacity == 24, 550, 360),
                         lambda_cell_to_pack =
                           ifelse(m$capacity == 24, 0, 240),
                         lambda_pack_to_ambient = 7.5,
                         lambda_pack_AC_to_ambient =
                           ifelse(m$model == "e-NV200", 2.0, 7.5),
                         fan_power =
                           ifelse(m$model == "e-NV200", 900, 0),
                         COP = ifelse(m$model == "e-NV200", 3.0, 0),
                         arrhenius_resistance = -3500,
                         heat_capacity = 1.0e6
                         )
  }
  m$modified.last.time <- now()
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
