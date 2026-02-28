#' Predict pack temperatures using a simple thermal model and data from
#' a LeafSpy logfile. Side effect: updates global_logtibble

global_logtibble <- NULL

#' @param effective_pack_resistance in Ohms
#' @param lambda_cell_to_pack time constant, in seconds
#' @param cell_heat_capacity in J/gK, total over all modules in pack
#' @param lambda_pack_to_ambient time constant, in seconds
#' @param logfile name of logfile (in .csv format), optional parameter
#'
#' @returns a logtibble with columns from original log, plus pack_avg_temp,
#'   pred_pack_avg_temp, soc, charging_kW
#'   n.b. the LeafSpy log doesn't record readings from all four thermosensors
#'   n.b. this function updates the value of global_logtibble
#' @export
#'
#' @examples
#' logtibble <- predict_temp(logfile = "monster")
#' logtibble <- predict_temp(logtibble, eff_pack_resistance = 3.9)
#' predict_temp(effective_pack_resistance = 3.5) # updates global_logtibble

#TODO: implement the pack-to-ambient model
#TODO: define a LeafSpyTibble class for tibbles translated from LeafSpy logs
#TODO: add a few parameters to model SOC-dependent entropic heating/cooling
#TODO: strip VIN column from csvs in data-raw/, for privacy, before release
#TODO: parse Lat and Long to numeric (or delete, for location-privacy)
#  stackoverflow.com/questions/69484220/
#    convert-dms-coordinates-to-decimal-degrees-in-r

# n.b. the temperature sensors are on the modules, not on the
# exterior of the pack -- so it's the heat capacity of the cells, of the
# module casing, and of the wiring within the modules, that'll be
# associated with the shortest time constant in a detailed thermal model.
# There'll be a temperature gradient to the exterior of the pack, another
# gradient to the frame it's bolted to, and yet-another gradient to the
# ambient air.  Even so: I think a two-stage thermal model will suffice,
# and unless I paste a thermosensor on the exterior of the pack and another
# nearby on the frame I very much doubt I could estimate any of the other
# time constants (given the collinearity of the time-constants and heat
# capacities, and given the confounding factors of entropic heat, vehicle
# speed, pavement temperature, etc)


library(tidyverse)
library(usethis)
library(xts)
library(janitor)

predict_temp <- function(effective_pack_resistance = 4,
                         lambda_cell_to_pack = 60,
                         cell_heat_capacity = 1.0e6,
                         lambda_pack_to_ambient = 10000,
                         logfile = "") {
  if (!nzchar(logfile)) {
    # logt is a required parameter if no logfile is specified
    stopifnot(!is.null(global_logtibble))
    #TODO: define a bespoke class for the logtibble, to assure conformance
    logtibble <- global_logtibble
  }
  else {
    logtibble <- read_csv(
      paste0("data-raw/", logfile, ".csv"),
      name_repair = "unique_quiet",
      col_types =
        cols(`Date/Time` =
               col_datetime("%d/%m/%Y %H:%M:%S"))
    )
    logtibble <- logtibble |>
      select(!starts_with("Debug")) |>
      janitor::clean_names()
    #n.b. there are multiple Debug cols in LeafSpy logs
    #scale soc to soc%
    logtibble <- logtibble |> mutate (soc = soc / 10000)

    #compute delta_t for runs of near-consecutive samples
    logtibble <- logtibble |>
      mutate(delta_t = date_time - lag(date_time))

    sampling_interval <- as.double(median(logtibble$delta_t, na.rm = TRUE))
    # multiple missing samples will terminate a predictive segment
    # isolated missing samples do not hugely affect our model's predictions
    # time-stamps in the logs have a precision of 1 second
    max_delta_t <- 2 * sampling_interval + 2
    logtibble <- logtibble |>
      mutate(delta_t = ifelse(delta_t > max_delta_t, NA, delta_t))

    # charging power (in kW)
    logtibble <- logtibble |>
      mutate(charging_kW = obc_out_pwr / 1000.0, .before = cp1)

    # annoyingly, pack_t3_c is uniformly NA in all my logfiles
    logtibble <- logtibble |>
      mutate(pack_avg_temp = rowMeans(across(c(
        pack_t1_c, pack_t2_c, pack_t4_c
      ))), .before = cp1)

    # rate of heat gain (in K/s), a very noisy but unbiased estimate.
    # n.b. there's no advantage in using a higher-order estimate, because
    # our model is an exponential smoothing and then a numeric integration
    # (cumsum) of these noisy deltas.
    logtibble <- logtibble |>
      mutate(delta_K_delta_t =
               (pack_avg_temp - lag(pack_avg_temp)) / delta_t,
             .before = cp1)

    global_filnm <<- logfile # for convenience when plotting
    #TODO: define a bespoke class to represent a logtibble and its metadata

  } # endif (!nzchar(logfile))

  # predicted Joule heating of cells (in W)
  logtibble <- logtibble |>
    mutate(
      pred_Joule_heating =
        pack_amps * pack_amps * effective_pack_resistance,
      .before = cp1
    )

  # unlagged predicted per-sample delta-heating of pack (in temperature K)
  logtibble <- logtibble |>
    mutate(pred_heating_unlagged =
             pred_Joule_heating / cell_heat_capacity,
           .before = cp1)

  # Note that it's possible to estimate the effective pack resistance from
  # a logfile if you have a plausible estimate for its heat capacity.
  # Alternatively you could estimate the effective heat capacity if you
  # have a plausible estimate for its effective resistance.  But
  # you can't estimate both, as it's the resistance divided by
  # the capacity that is used to estimate instantaneous "Joule heating" from
  # the square of the pack's instantaneous amperage.

  # recompute the sampling_interval with more precision, by using the mean
  # rather than the median as we had done during pre-processing the logfile.
  # This calculation ignores the long intervals between segments, and is likely
  # to be non-integral; whereas the median is always an integer (due to the
  # 1-second resolution of timestamps as recorded in the logfile)
  sampling_interval <- as.double(mean(logtibble$delta_t, na.rm = TRUE))

  # we use an Exponential Moving Average filter on a per-sample basis:
  # rather inaccurate when there are missing samples; but efficiently computed,
  # and undistorted by roundoff errors in the time-stamps on the samples
  EMA_parameter_cell_to_pack <- sampling_interval / lambda_cell_to_pack

  # we now lurch from the tidyverse into the wilds of xts, to apply a
  # a simple recursive filter -- without manually converting the recursion
  # into an iteration (e.g. with sapply()) or running an element-by-element
  # recursion in R's interpreter.
  # github.com/tidyverse/dbplyr/issues/1108

  # predicted delta-heating of pack (in K), with exponential lag
  w <- which(is.na(logtibble$delta_t)) # group by gaps in sampling
  unlagged_heat <- as.xts(logtibble$pred_heating_unlagged, logtibble$date_time)
  lagged_heat <- as.xts(vector(mode = "double",
                               length = length(unlagged_heat)),
                        logtibble$date_time)
  wend <- c(w, length(unlagged_heat) + 1) # append a 0-length "ghost segment"
  for (i in seq(length(w))) {
    # iterate over all non-ghost segments
    lagged_heat[wend[i]:(wend[i + 1] - 1)] <-
      stats::filter(
        unlagged_heat[wend[i]:(wend[i + 1] - 1)] *
          EMA_parameter_cell_to_pack,
        1. - EMA_parameter_cell_to_pack,
        method = "recursive",
        init = 0.
      )
  }

  pack_avg_temp_xts <-
    xts(logtibble$pack_avg_temp, logtibble$date_time)

  # predict pack temps from cumsum of lagged delta-heat in each segment
  segment_starts <- logtibble$date_time[w]
  pred_pack_avg_temp_xts <- lag(lagged_heat)
  # the first point in each segment is an observation, not a prediction
  pred_pack_avg_temp_xts[segment_starts] <- NA

  for (i in seq(length(w))) {
    starting_temp <- as.double(pack_avg_temp_xts[w[i]])
    pred_pack_avg_temp_xts[(wend[i] + 1):(wend[i + 1] - 1)] <-
      cumsum(pred_pack_avg_temp_xts[(wend[i] + 1):(wend[i + 1] - 1)]) +
      starting_temp
  }

  # we now apply a second exponential filter, with a much longer time
  # constant, to (very roughly) model the cooling of the pack by its
  # convection and conduction to the ambient air (and also to the
  # vehicle's frame, which is assumed to be in thermal equilibrium
  # with the ambient temperature as measured by a thermosensor at
  # the front of the vehicle.
  ambient_xts <- as.xts(logtibble$ambient, logtibble$date_time)
  EMA_parameter_pack_to_ambient <- sampling_interval / lambda_pack_to_ambient
  lagged_heat <- as.xts(vector(mode = "double",
                               length = length(unlagged_heat)),
                        logtibble$date_time)
  for (i in seq(length(w))) {
    # compute a lagged time-series of pack-to-ambient temperature differentials.
    # These are proportional to heats in J, with the constant of proportionality
    # being the heat capacity of the pack excluding the modules and their
    # contents.  If the high-amp cabling to the modules is heating significantly,
    # this wattage will be lumped with the Joule heating of the cells, and the
    # time-constant of its decay will be lumped with the (short) time-constant
    # of the modules' thermosensor coming into equilibrium with its cells.
    starting_temp <- as.double(pack_avg_temp_xts[w[i]])
    lagged_heat[(wend[i] + 1):(wend[i + 1] - 1)] <-
      stats::filter(
        lagged_heat[(wend[i] + 1):(wend[i + 1] - 1)] +
          (ambient_xts[(wend[i] + 1):(wend[i + 1] - 1)] -
             pred_pack_avg_temp_xts[(wend[i] + 1):(wend[i + 1] - 1)]) *
          EMA_parameter_pack_to_ambient,
        1. - EMA_parameter_pack_to_ambient,
        method = "recursive",
        init = starting_temp - ambient_xts[wend[i]]
        )
  }
  pred_pack_avg_temp_xts <- pred_pack_avg_temp_xts + lagged_heat
  # n.b. the computation above does not model a second-order effect: diffusion
  # of the lagged_heat to the ambient.  This could be handled with a slightly
  # more complex recursive filter, but I can't see how to implement this within
  # the constraints of stats::filter(). R's runtime is not well-suited to
  # recursion over long vectors -- so we'd either have to be very tolerant of a
  # long runtime, find a more general interface to a recursive filter
  # implemented in a language that efficiently handles recursion, or run a third
  # stats::filter() to model this second-order effect (and hope that the
  # unmodelled third-order effect isn't significant for our datasets)

  #todo: estimate the magnitude of the second-order effect, by modelling it
  #with an additional stats::filter()

  # return to the tidyverse!
  logtibble <- logtibble |>
    mutate(pred_pack_avg_temp = as.vector(pred_pack_avg_temp_xts),
           .before = "cp1") |>
    select(!c(pred_Joule_heating, pred_heating_unlagged))

  global_logtibble <<- logtibble
  # a hack which causes nlm() to report its fit as a tbl; also convenient
  # for repeated calls of predict_temp() from the console, as global_logtibble
  # is its default input

  return(logtibble)

}
