#' Munges a csv to ensure it has no VIN column, has timestamps in ISO format,
#' is sorted on these timestamps, and has tidied column names.  Side effect:
#' updates this csv file.  Returns a thmodel with the munged csv as its logdata,
#' and with provenance fields set.
#'
#' @param logfilnm  name of logfile to be interpreted
#' @param logfildir directory of csv logfile, optional, default is "data-raw"
#' @param logname friendly and informative name for this logfile, optional
#'
#' @returns thmodel
#' @export
#'
#' @examples
#' m <- munge_logfile()

munge_logfile <- function(logfilnm = "log26Jan2026.csv",
                          logfildir = "data-raw",
                          logname = NULL) {
  #todo: ? default logfildir to "extdata" ? See https://r-pkgs.org/data.html

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
      name_repair = "unique",
      col_types = cols(`Date/Time` = col_datetime("%d/%m/%Y %H:%M:%S")),
      show_col_types = FALSE
    )
  }

  #todo: sanity-check any csv, to assure that it is fit for purpose
  #as input to our code.  Any rev to LeafSpy, or any difference in its
  #setup, might produce incompatible csv files.

  tbl <- tbl |>

    #n.b. there may be multiple Debug cols in LeafSpy logs, causing
    #annoying warning messages when they're imported into the tidyverse
    select(!starts_with("Debug")) |>

    janitor::clean_names() |>

    #n.b. LeafSpy writes "none" in the odo_km field, when the car is
    #not in Drive mode. These entries are interpreted as 0 by read_csv, but
    #NA is more accurate.
    mutate(odo_km = ifelse(odo_km == 0, NA, odo_km)) |>

    arrange(date_time)

  if (!already_munged) {
    #n.b. publishing a VIN is hazardous, because it's sometimes used as a
    #self-authenticating identifier ("security by obscurity") and also
    #because it is PII of no relevance to the analysis.

    #defensive guard: the user may have manually removed VIN
    if ("VIN" %in% names(tbl)) {
      tbl <- tbl |> select(!VIN)
    }

    #todo: mutate to a securely hashed VIN. ?keyed to what secret?

    #n.b. publishing high-resolution lat/long data is a privacy
    #hazard which can be mitigated by munging, without impairing
    #the accuracy of a thermal model.
    latv <- select(tbl, lat)
    regexpv <- rep("[.]", length(latv))
    # we retain one decimal point of accuracy in the minutes
    # n.b. LeafSpy writes lat/long data as strings in "degree minute" format
    # this is very untidy code
    ft <- function(x) substr(x,1,str_locate(x,"[.]") + 1 )
    latt <- ft(latv[[1]])
    longt <- ft(select(tbl, long)[[1]])
    tbl <- tbl |> mutate(lat = latt, long = longt)

    #write the munged file to disk
    write_csv(tbl, logfilpath)
  }

  m <- new_thmodel()
  m$name <- ifelse(is.null(logname), str_sub(logfilnm, 1, (nchar(logfilnm) - 4)), logname)
  m$filnm <- logfilnm
  m$fildir <- logfildir
  m$logdata <- tbl

  #todo: consider writing m to data/name.rda, to optimise reloads of
  #csv from data-raw/. Downside: the user must specify "data" as the logfildir
  #in future, to benefit from this optimisation. Downside: this might cause
  #confusion about versioning of thmodel objects and csv files.

  return(m)
}
