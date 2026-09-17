#' Datasets for testing ThermalEV
#'
#' eNV50kWh: a very large thmodel object, with provenance.  The fields are
#' as defined in thmodel.R.  The logdata is a tibble with 27922 rows
#' and 185 columns.  It is constructed by concatenating (using rbind)
#' the logdata in eNV200noac50kWh and eNV200ac50kWh.
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
#' @source {thmodel} R package.
"eNV50kWh"

#' Leaf24kWh: all my LeafSpy records from a 2013 24kWh Leaf
#' @format A thmodel object
"Leaf24kWh"

#' ocv_tbl_50: a lookup tibble of SOC onto OCV, empirically derived for the
#' 50kWh pack (with extremal values from the cell manufacturer's specs)
#' @format 51 obs of 2 variables
#' @source {thmodel} R package.
"ocv_tbl_50"

#' eNV200noac50kWh: a thmodel for my 50kWh e-NV200 before its aircon
#' was regassed -- so it had no active cooling of its pack.
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
#' @source {thmodel} R package.
"eNV200noac50kWh"

#' eNV200ac50kWh: a thmodel object containing logs from the fully-functional
#' 50kWh upgrade of my eNV200.
#' @format A thmodel object.
#' @source {thmodel} R package.
"eNV200ac50kWh"

#' eNV24kWh: a very large thmodel object containing logs from my eNV200
#' prior to the replacement of its 24kWh pack.
#' @format A thmodel object.
#' @source {thmodel} R package.
"eNV24kWh"

#' eNV200ac24kWh_2025: a thmodel object of logs from 2025
#' @format A thmodel object.
#' @source {thmodel} R package.
"eNV200ac24kWh_2025"

#' eNV200ac24kWh_2025mdy: a thmodel object of logs from 2025 when
#' my LeafSpy was configured to store timestamps in USonian format.
#' @format A thmodel object.
#' @source {thmodel} R package.
"eNV200ac24kWh_2025mdy"

#' eNV200ac24kWh_2024: a thmodel object of logs from 2024
#' @format A thmodel object.
#' @source {thmodel} R package.
"eNV200ac24kWh_2024"

#' eNV200ac24kWh_2023: a thmodel object of logs from 2023
#' @format A thmodel object.
#' @source {thmodel} R package.
"eNV200ac24kWh_2023"

#' eNV200ac24kWh_2022: a thmodel object of logs from 2022
#' @format A thmodel object.
#' @source {thmodel} R package.
"eNV200ac24kWh_2022"

#' eNV200ac24kWh_2021: a thmodel object of logs from 2021
#' @format A thmodel object.
#' @source {thmodel} R package.
"eNV200ac24kWh_2021"

#' eNVa50kWh_2025: a thmodel object of Alastair's logs from his 50kWh
#' eNV200.
#' @format A thmodel object.
#' @source {thmodel} R package.
"eNVa50kWh_2025"

#' eNVa24kWh_2024: a thmodel object of Alastair's logs from his 24kWh
#' eNV200, in 2024.
#' @format A thmodel object.
#' @source {thmodel} R package.
"eNVa24kWh_2024"

#' Leaf24kWh_2019: a thmodel object of logs from my 24kWh Leaf in 2019
#' @format A thmodel object.
#' @source {thmodel} R package.
"Leaf24kWh_2019"

#' Leaf24kWh_2021: a thmodel object of logs from my 24kWh Leaf in 2021
#' @format A thmodel object.
#' @source {thmodel} R package.
"Leaf24kWh_2021"

#' Leaf24kWh_2022: a thmodel object of logs from my 24kWh Leaf in 2022
#' @format A thmodel object.
#' @source {thmodel} R package.
"Leaf24kWh_2022"






