library(tidyverse)
library(usethis)
library(xts)
library(janitor)

#' Munges a csv to ensure it has no VIN column, has timestamps in ISO format,
#' and has tidied column names.  Side effect: updates this csv file.  Returns
#' a thmodel with this csv as its logdata, and with provenance fields set.
#'
#' @param logfildir
#' @param logfilnm
#'
#' @returns thmodel
#' @export
#'
#' @examples
#' m <- munge_logfile()

munge_logfile <- function(logfildir = "data-raw",
                          logfilnm = "log26Jan2026.csv") {

    # read a csv file, create a new thmodel object, possibly write a munged csv
    logfilpath <- paste0(here::here(logfildir), "/", logfilnm)

    # read headers to determine if it's already munged
    tbl <- read_csv(
      logfilpath,
      name_repair = "unique_quiet",
      n_max = 1,
      show_col_types = FALSE
    )
    already_munged <- !("VIN" %in% names(tbl))

    if (already_munged) {
      tbl <- read_csv(logfilpath,
                      name_repair = "unique_quiet",
                      show_col_types = FALSE)
    } else {
      tbl <- read_csv(
        logfilpath,
        name_repair = "unique_quiet",
        col_types = cols(`Date/Time` = col_datetime("%d/%m/%Y %H:%M:%S")),
        show_col_types = FALSE
      )
    }

    if (!already_munged) {
      tbl <- tbl |>
        select(!starts_with("Debug")) |>
        #n.b. there may be multiple Debug cols in LeafSpy logs

        select(!VIN) |>
        #n.b. publishing a VIN is hazardous, because it's sometimes used as a
        #self-authenticating identifier ("security by obscurity") and also
        #because it is PII of no relevance to the analysis.

        #todo: add a pseudoVIN, and other descriptive information about
        #the vehicle, to metadata fields in a bespoke logtibble class.

        janitor::clean_names()

      #TODO: convert Lat and Long to double (or delete, for location-privacy)
      #  stackoverflow.com/questions/69484220/
      #    convert-dms-coordinates-to-decimal-degrees-in-r

      #write the munged file to disk
      write_csv(tbl, logfilpath)

    } else {
      #todo: sanity-check any VIN-less csv, to assure that it is fit for purpose
      #as input to our code.  Any rev to LeafSpy, or any difference in its
      #setup, might produce csv files that are incompatible with our code.
    }

    m <- new_thmodel()
    m$name <- str_sub(logfilnm, 1, (nchar(logfilnm) - 4)) # strip extension
    m$filnm <- logfilnm
    m$fildir <- logfildir
    m$logdata <- tbl

    #todo: carefully consider writing m to data/name.rda, to optimise reloads of
    #csv from data-raw/. Downside: the user must specify "data" as the logfildir
    #in future, to benefit from this optimisation. Downside: this might cause
    #confusion about versioning of thmodel objects and csv files.

  return(m)
}
