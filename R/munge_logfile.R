#' Munges a csv to ensure it has no VIN column, has timestamps in ISO format,
#' is sorted on these timestamps, and has tidied column names.  Side effect:
#' updates this csv file.  Returns a thmodel with the munged csv as its logdata,
#' and with provenance fields set.
#'
#' Todo: munge the filenm, if it contains information from a VIN.  Perhaps:
#' allow the user to specify a pseudoVIN, to retain provenance informmation
#' on csv files collected from multiple vehicles.
#'
#' @param logfilnm  name of logfile to be interpreted
#' @param logfildir directory of csv logfile, optional, default is "data-raw"
#' @param logname friendly and informative name for this logfile, optional
#' @param USonian_dates TRUE: mdy parsing; FALSE (default): dmy parsing
#' @returns thmodel
#' @export
#'
#' @examples
#' m <- munge_logfile()

munge_logfile <- function(logfilnm = "log26Jan2026.csv",
                          logfildir = "data-raw",
                          logname = NULL,
                          USonian_dates = FALSE) {
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
                    # to cope with the occasional "na"
                    col_types = cols(
                      '12v Bat Volts' = col_character(),
                      '12v Bat Amps' = col_character(),
                      `GPS Status` = col_character() # four bits, in hex format
                    ),
                    show_col_types = FALSE)
  } else {
    tbl <- read_csv(
      logfilpath,
      name_repair = "unique_quiet",
      # n.b. dates in LeafSpy csv files are deeply ambiguous for the first
      # twelve days of each month; and later dates in the month will cause the
      # conversion routines of read_csv to return NA values if you "guess" the
      # wrong value for the USonian_dates flag.  A plot_log will reveal
      # inversions in what should be a strictly increasing set of odometer
      # readouts in a set of csv files from the same vehicle.
      col_types = cols(
        `Date/Time` = col_datetime(
          ifelse(USonian_dates, "%m/%d/%Y%.%H:%M:%S", "%d/%m/%Y%.%H:%M:%S")
        ),
        '12v Bat Volts' = col_character(),
        '12v Bat Amps' = col_character(),
        'GPS Status' = col_character()
      ),
      show_col_types = FALSE
    )
  }

  # todo: add support for coping with variations in LeafSpy settings
  stopifnot(!is.na(tbl$'Date/Time'[1])) # USonian mdy?
  stopifnot("Odo(km)" %in% names(tbl)) # Imperial miles?

  tbl <- tbl |>

    # n.b. there are multiple Debug cols in my LeafSpy logs, causing
    # annoying warning messages when they're imported into the tidyverse
    select(!starts_with("Debug")) |>
    arrange('Date/Time')

  if (!already_munged) {
    # n.b. publishing a VIN is hazardous, because it's sometimes used as a
    # self-authenticating identifier ("security by obscurity") and also
    # because it is PII of no relevance to the analysis.

    #defensive guard: the user may have manually removed VIN
    if ("VIN" %in% names(tbl)) {
      tbl <- tbl |> select(!VIN)
    }

    # todo: could mutate to a securely hashed VIN, but keyed to what secret?

    # n.b. publishing high-resolution lat/long data is a privacy
    # hazard which can be mitigated by munging, without impairing
    # the accuracy of a thermal model.
    latv <- select(tbl, Lat)
    regexpv <- rep("[.]", length(latv))
    # we retain one decimal point of accuracy in the minutes
    # n.b. LeafSpy writes lat/long data as strings in "degree minute" format
    # this is very untidy code
    ft <- function(x) substr(x,1,str_locate(x,"[.]") + 1 )
    latt <- ft(latv[[1]])
    longt <- ft(select(tbl, Long)[[1]])
    tbl <- tbl |> mutate(Lat = latt, Long = longt)

    #write the munged file to disk
    write_csv(tbl, logfilpath)
  }

  zct <- sum(tbl$Gids == 0, na.rm = TRUE)
  if (zct > 0)
    warning(paste0(logfilnm, ": ", zct, " zero",
                   ifelse(zct>1,"s",""), " in the GIDs column."))
  zct <- sum(tbl$'Pack Volts' == 0, na.rm = TRUE)
  if (zct > 0)
    warning(paste0(logfilnm, ": ", zct, " zero",
                   ifelse(zct>1,"s",""), " in the Pack Volts column."))

  tbl <- tbl |>
    # Name cleaning: required for entry into the tidyverse.
    janitor::clean_names() |>
    # Data cleaning: LeafSpy writes "none" in the odo_km field, when the car is
    # not in Drive mode. These entries are interpreted as 0 by read_csv, but NA
    # is desirable for analysis.  LeafSpy sometimes writes "na" in the 12V Bat
    # Amps field, which we interpret as NA.  There's an occasional spurious 0 in
    # at least two other columns.

    # TODO: the x12v_bat_amps repair (below) doesn't work until its name is
    # cleaned by a read_csv "unique-quiet".  The error clears if you re-run
    # thmodel_from_directory(), but ideally would be cleared below.
    mutate(
      odo_km = ifelse(odo_km == 0, NA, odo_km),
      x12v_bat_amps =
        ifelse(x12v_bat_amps == "na", NA,
               suppressWarnings(as.double(x12v_bat_amps))),
      gids = ifelse(gids == 0, NA, gids),
      pack_volts = ifelse(pack_volts == 0, NA, pack_volts)
    )

  m <- new_thmodel()
  m$name <- ifelse(is.null(logname), str_sub(logfilnm, 1, (nchar(logfilnm) - 4)), logname)
  m$filnm <- logfilnm
  m$fildir <- logfildir
  m$logdata <- tbl
  return(m)
}
