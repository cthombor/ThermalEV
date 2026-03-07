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
  m <- structure(list(), class = "thmodel")
  m <-
    list(
      name = "",
      # user-friendly name, used for titles of plots
      filnm = "",
      # provenance file (a csv filnm)
      fildir = "",
      # provenance location (relative to a homedir), no trailing "/"
      modified.first.time = now(), # named as in xts
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
    m$parameters <- list(effective_pack_resistance = 400,
                         lambda_cell_to_pack = 100,
                         lambda_pack_to_ambient = 8,
                         heat_capacity = 1.0e6)
  }
  return(m)
}

#todo: add a validator, at minimum confirming the presence of named elements of
#the required class, possibly also doing some sanity-checking e.g. on the names
#in logdata (if it has zero rows).

#todo: write getters and setters, with the setters updating modified.last.time
#if modifying the value of a field

#todo: write helpers for summary and plot
