#' Datasets for testing ThermalEV
#'
#' eNV200noac50kWh: a large thmodel object, with provenance.  The fields are
#' as defined in thmodel.R.  The logdata is a tibble with more than 12000 rows
#' and more than 150 columns.
#' @format A thmodel object.  Fields in the logdata include
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
"eNV200noac50kWh"

#' eNV200ac24kWh_2025: a thmodel object, with provenance.  The fields are
#' as defined in thmodel.R.  The logdata is a tibble with more than 3600 rows
#' and more than 150 columns.
#' @format A thmodel object.
#'
#' @source {thmodel} R package.
"eNV200ac24kWh_2025"
