#' Uses a 3-parameter thermal model to predict temperatures in a LeafSpy log
#'
#' A LeafSpy logfile may be specified by filename and directory, in which case
#' this logfile is munged -- to mitigate the privacy risk of publishing a VIN,
#' and to revise column names so that they're tidy.  In normal use, the logfile
#' is specified to predict_temp() by a thmodel object -- which has metadata
#' describing its provenance and the values of the modelling parameters, and
#' which has a tibble-translation of the orginal LeafSpy csv with additional
#' columns for the predictions of the model and for convenience when plotting.
#'
#' @param tmodel, a thmodel
#' @param effective_pack_resistance, a primary factor (in Ohms)
#' @param lambda_cell_to_pack, a primary factor (in seconds)
#' @param lambda_pack_to_ambient, a primary factor (in seconds)
#' @param heat_capacity, a secondary factor (in J/K)
#' @param logfilnm, optional parameter, required when tmodel == NULL
#' @param logfildir, optional parameter
#'
#' @returns a thmodel
#' @export
#'
#' @examples
#' m <- predict_temp(new_thmodel()) # uses data-raw/log26Jan26.csv
#' m <- m |> predict_temp(effective_pack_resistance = 5)

predict_temp <- function(tmodel = NULL,
                         effective_pack_resistance = 4.,
                         lambda_cell_to_pack = 1.0e2,
                         lambda_pack_to_ambient = 1.0e4,
                         heat_capacity = 1.0e6,
                         logfilnm = "log26Jan2026",
                         logfildir = "data-raw") {

  if (!nzchar(logfilnm) && is.null(tmodel)) {
    stop("Aborting. Please specify the name of a LeafSpy logfile.")
  }
  else {
    if (!is.null(tmodel)) {
      m <- tmodel
    } else {
      m <- new_thmodel()
    }
    if ((m$filnm != logfilnm) || (m$fildir != logfildir)) {
      # read a different logfile, and munge it (if isn't already munged)
      m <- munge_logfile(logfilnm = logfilnm, logfildir = logfildir)
    }
  } # endif (!nzchar(logfile))

  logtibble <- m$logdata



  if (!"delta_t" %in% names(logtibble)) {
    # avoid recomputing these columns

    #compute delta_t for runs of near-consecutive samples
    logtibble <- logtibble |>
      mutate(delta_t = date_time - lag(date_time))

    sampling_interval <- as.double(median(logtibble$delta_t, na.rm = TRUE))
    # multiple missing samples will terminate a predictive segment
    # n.b. isolated missing samples do not hugely affect our model's predictions
    # time-stamps in the logs have a precision of 1 second
    max_delta_t <- 2 * sampling_interval + 2
    logtibble <- logtibble |>
      mutate(delta_t = ifelse(delta_t > max_delta_t, NA, delta_t))

    # pack_t3_c is uniformly NA in all my logfiles!
    logtibble <- logtibble |>
      mutate(pack_avg_temp = rowMeans(across(c(
        pack_t1_c, pack_t2_c, pack_t4_c
      ))), .before = cp1)

    # rate of heat gain (in K/s)
    logtibble <- logtibble |>
      mutate(delta_K_delta_t =
               (pack_avg_temp - lag(pack_avg_temp)) / delta_t,
             .before = cp1)

    # charging power (in kW)
    logtibble <- logtibble |>
      mutate(charging_kW = obc_out_pwr / 1000.0, .before = cp1)

    # pack power (in kW)
    logtibble <- logtibble |>
      mutate(pack_kW = pack_volts * abs(pack_amps) / 1000.0, .before = cp1)

    #munge soc to a percentage, for ease of plotting and by convention
    logtibble <- logtibble |> mutate (soc = soc / 10000)

  }

  # we now predict temperatures, using the parameters

  # predicted Joule heating of cells (in W)
  logtibble <- logtibble |>
    mutate(
      pred_Joule_heating =
        pack_amps * pack_amps * effective_pack_resistance,
      .before = cp1
    )

  # unlagged predicted per-sample delta-heating of pack (in temperature K),
  # based on our roughly-estimated heat_capacity. If this parameter is
  # modified, then the best-fit value of effective_pack_resistance is affected
  # (precisely in inverse proportion), since it is the ratio between these two
  # parameters which is the constant of proporationality between the square of
  # pack amperage and its "Joule heating" in units of K/s.

  logtibble <- logtibble |>
    mutate(pred_heating_unlagged =
             pred_Joule_heating / heat_capacity,
           .before = cp1)

  # we now recompute the sampling_interval with more precision, by using the
  # mean rather than the median as we had done when defining segments in the
  # logfile pre-processing step. This calculation is insensitive to the long
  # intervals between segments, and is likely is at a higher precision than the
  # 1-second resolution of timestamps as recorded in the logfile)
  sampling_interval <- as.double(mean(logtibble$delta_t, na.rm = TRUE))

  # we use an Exponential Moving Average filter on a per-sample basis: rather
  # inaccurate when there are missing samples; but can be efficiently computed
  # on any modern (deeply-pipelined) CPU.  Furthermore, it is undistorted by the
  # roundoff errors in the time-stamps (at 1-second precision!) on the samples
  EMA_parameter_cell_to_pack <- sampling_interval / lambda_cell_to_pack

  # we now lurch from the tidyverse into the wilds of xts. The EMA is a simple
  # recursive filter; but R's runtime is hostile to recursion.  It's possible to
  # use sapply() to implement a reasonably-efficient tail-recursion, but its
  # correctness relies on an undocumented expectation of sapply: that it
  # performs a sequential in-order execution of its FUN which starts at the
  # first element of its input vector(s). See
  # stackoverflow.com/questions/49348870/tibbletime-previous-days-close/
  # 49373709#comment140995212_49373709 and
  # github.com/tidyverse/dbplyr/issues/1108

  # predicted delta-heating of pack (in K), with exponential lag, grouped
  # by the gaps in the sampling
  w <- which(is.na(logtibble$delta_t))
  unlagged_heat <- as.xts(logtibble$pred_heating_unlagged, logtibble$date_time)
  lagged_heat <- as.xts(vector(mode = "double",
                               length = length(unlagged_heat)),
                        logtibble$date_time)
  wend <- c(w, length(unlagged_heat) + 1) # append a 0-length "ghost segment"

  for (i in seq(length(w))) {
    # apply the EMA iteratively, over all non-ghost segments
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
  # n.b. the first point in each segment is an observation, not a prediction
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
  lagged_heat <- as.xts(vector(mode = "double", length = length(unlagged_heat)),
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
         (ambient_xts[(wend[i] + 1):(wend[i + 1] - 1)] -
             pred_pack_avg_temp_xts[(wend[i] + 1):(wend[i + 1] - 1)]) *
          EMA_parameter_pack_to_ambient,
        1. - EMA_parameter_pack_to_ambient,
        method = "recursive",
        init = starting_temp - ambient_xts[wend[i]]
      )
  }
  pred_pack_avg_temp_xts <- pred_pack_avg_temp_xts + lagged_heat

  # return to the tidyverse!  Hooray!!
  logtibble <- logtibble |>
    mutate(pred_pack_avg_temp = as.vector(pred_pack_avg_temp_xts),
           .before = "cp1") |>
    select(!c(pred_Joule_heating, pred_heating_unlagged))

  m$logdata <- logtibble
  m$filnm <- logfilnm
  m$fildir <- logfildir
  m$parameters <- list(effective_pack_resistance = effective_pack_resistance,
                    lambda_cell_to_pack = lambda_cell_to_pack,
                    lambda_pack_to_ambient = lambda_pack_to_ambient,
                    heat_capacity = heat_capacity)
  m$modified.last.time <- now()

  return(m)

}
