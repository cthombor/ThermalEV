# thmodel: S3 class constructors and helpers

library(tidyverse)
library(xts)

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
      # provenance (a csv filnm without a .3 ".csv" extension)
      fildir = "",
      # provenance relative to a user-varying path, no trailing "/"
      modified.first.time = now(), # named as in xts
      modified.last.time = now(),
      logdata = tibble(0),
      # if logdata has more than one column, then this must be a log from
      # LeafSpy as munged by munge_logfile, with additional columns
      # from thmodel.predict_temp() on the parameters
      parameters = list(NA),
      # if first element !is.NA, logdata must have the predicted temps; if the
      # first element of fit !is.NA, then these parameters must be a best-fit,
      # as found by nlm().
      #todo: refine this class to record any non-default values for nlm() params
      fit = list(NA)
      # if the first element !is.NA, this must be the result of an nlm()
    )
  return(m)
}

#todo: add a validator, at minimum confirming the presence of named elements of
#the required class, possibly also doing some sanity-checking e.g. on the names
#in logdata (if !is.NA).

#todo: write getters and setters, with the setters updating modified.last.time
#if modifying the value of a field

#todo: write helpers for summary and plot
