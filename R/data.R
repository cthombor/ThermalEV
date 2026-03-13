#' Datasets for testing ThermalEV
#'
#' eNV200noac50kWh: a large thmodel object, with provenance.  The fields are
#' as defined in thmodel.R.  The logdata is a tibble with more than 12000 rows
#' and more than 150 columns.
#' @format A thmodel object.  Fields in the logdata include
#'   * {date_time}{timestamp in POSIXct format}
#'   * {elv}{elevation in meters above sea level, an integer}
#'   * {soc}{state of charge, an integer in the range 0:1e6}
#'   * {pack_volts}{pack voltage, at 0.1 volt precision}
#'   * {pack_amps}{pack amperage, at 0.001 amp precision}
#'   * {pack_t1_c}{temperature at surface of module 1, at 0.1 K precision}
#'   * {pack_t2_c}{temperature at surface of module 2, at 0.1 K precision}
#'   * {pack_t3_c}{uniformly NA in our version of LeafSpy}
#'   * {pack_t4_c}{temperature at surface of module 4, at 0.1 K precision}
#'   * {odo_km}{odometer reading, if car in Drive mode, otherwise NA}
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
