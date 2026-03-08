#' Large dataset for testing ThermalEV
#'
#' A dataset containing a large thmodel object, its provenance, and its fitting
#' parameters.  The fields of the thmodel object are as defined in thmodel.R.
#' The logdata is a tibble with more than 12000 rows and more than 150 columns.
#' Most of the columns are raw data from a LeafSpy csv file; however a few have
#' been added; and some have been munged to preserve privacy, and to transform
#' datestamps from "%d/%m/%Y %H:%M:%S" format to POSIXct format.  Column names
#' from the LeafSpy csv have been morphed for convenient reference within R, e.g
#' "Date/Time" is morphed to "date_time".
#'
#' @format A thmodel object containing a large tibble.  Notable columns:
#' \describe{
#'   \item {date_time}{timestamp in POSIXct format}
#'   \item {elv}{elevation in meters above sea level, an integer}
#'   \item {soc}{state of charge, an integer in the range 0:1e6}
#'   \item {pack_volts}{pack voltage, at 0.1 volt precision}
#'   \item {pack_amps}{pack amperage, at 0.001 amp precision}
#'   \item {pack_t1_c}{temperature at surface of module 1, at 0.1 K precision}
#'   \item {pack_t2_c}{temperature at surface of module 2, at 0.1 K precision}
#'   \item {pack_t3_c}{uniformly NA in our version of LeafSpy}
#'   \item {pack_t4_c}{temperature at surface of module 4, at 0.1 K precision}
#'   \item {odo_km}{odometer reading, if car in Drive mode, otherwise NA}
#' }
#'
#' @source {thmodel} R package.
