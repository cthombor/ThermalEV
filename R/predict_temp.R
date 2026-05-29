#' Uses a 5-parameter thermal model to predict temperatures in a LeafSpy log
#'
#' A LeafSpy logfile may be specified to this function by filename and
#' directory, in which case this logfile is munged -- to mitigate the privacy
#' risk of publishing a VIN, and to revise column names so that they're tidy.
#' This routine also does some "cleaning" of obviously-wonky data e.g. of a
#' pack at 0 Volts or at 80 degrees (in a field that is normally in Centrigade
#' units).  The output of this routine is a thmodel object containing the
#' munged and cleaned data from LeafSpy augmented with additional columns for
#' the predictions of pack temperature (and for convenience when plotting).
#' The thmodel has metadata describing its provenance and the (updated)
#' values of the modelling parameters used for its temperature predictions.
#'
#' Notes on heat capacity:
#'
#' The 96 cells in my aftermarket 50kWh pack weigh 2.13 kg apiece, so there's a
#' total of roughly 200kg of water (at 4.13 J/gK) in the electrolyte.  There'll
#' be some additional heat content in the 100kg of non-cell contents in the
#' pack. Polyethylene is 2.0 J/gK, steel is 0.5 J/gK, everything else is less.
#' As a round number, the heat content of the pack is thus 1.0e6 J/K.  This is a
#' secondary parameter in our modelling because the Joule heating (in K) of a
#' pack is the square of its amperage, multiplied by its effective resistance
#' and divided by its heat capacity (in J/K).  The best-fit COP for the
#' heatpump is within the normal range for a well-engineered heatpump,
#' suggesting that the heat content of the pack is plausibly estimated
#' by our 1e6 J/K default.  However our mOhms value is linearly dependent
#' on the value assigned to the heat capacity of the pack, so it is a biased
#' estimate until such time (if ever) we have some way to estimate the pack's
#' effective heat capacity from empirical data.
#'
#' @param tmodel a thmodel, optional
#' @param effective_pack_resistance in mOhms, a primary parameter
#' @param lambda_cell_to_pack in seconds, a primary parameter
#' @param lambda_pack_to_ambient in hours, a primary parameter
#' @param lambda_pack_AC_to_ambient in hours, a primary parameter
#' @param lambda_cooling_power in seconds, a primary parameter
#' @param COP dimensionless, a primary parameter
#' @param heat_capacity in J/K, a secondary parameter
#' @param min_segment_length shorter sequences of samples are ignored
#' @param logfilnm name of a csv logfile to be read, if is.null(tmodel)
#' @param logfildir directory in which the logfile is located
#'
#' @returns a thmodel
#' @export
#'
#' @examples
#' m <- predict_temp() # uses data-raw/log26Jan26.csv
#' m <- m |> predict_temp(effective_pack_resistance = 0.5)

predict_temp <- function(tmodel = NULL,
                         effective_pack_resistance = NA,
                         lambda_cell_to_pack = NA,
                         lambda_pack_to_ambient = NA,
                         lambda_pack_AC_to_ambient = NA,
                         lambda_cooling_power = NA,
                         COP = NA,
                         heat_capacity = NA,
                         min_segment_length = 50,
                         logfilnm = "log26Jan2026.csv",
                         logfildir = "data-raw") {

  if (!nzchar(logfilnm) && is.null(tmodel)) {
    stop("Aborting. Please specify the name of a LeafSpy logfile.")
  }
  else {
    m <- tmodel
    if (is.null(m) || m$name == "") {
      m <- munge_logfile(logfilnm = logfilnm, logfildir = logfildir)
    }
  } # endif (!nzchar(logfile))

  if (length(m$parameters) == 0) {
    m <- default_params(m)
  }
  logtibble <- m$logdata

  # param values specified in the method call have precedence. Side effect:
  # if m$parameters is malformed, throw a "subscript out of bounds" error
  if (!is.na(effective_pack_resistance)) {
    m$parameters[["effective_pack_resistance"]] <- effective_pack_resistance
  }
  if (!is.na(lambda_cell_to_pack)) {
    m$parameters[["lambda_cell_to_pack"]] <- lambda_cell_to_pack
  }
  if (!is.na(lambda_pack_to_ambient)) {
    m$parameters[["lambda_pack_to_ambient"]] <- lambda_pack_to_ambient
  }
  if (!is.na(lambda_pack_AC_to_ambient)) {
    m$parameters[["lambda_pack_AC_to_ambient"]] <- lambda_pack_AC_to_ambient
  }
  if (!is.na(lambda_cooling_power)) {
    m$parameters[["lambda_cooling_power"]] <- lambda_cooling_power
  }
  if (!is.na(COP)) {
    m$parameters[["COP"]] <- COP
  }
  if (!is.na(heat_capacity)) {
    m$parameters[["heat_capacity"]] <- heat_capacity
  }


  # read a full set of params
  effective_pack_resistance <- m$parameters[["effective_pack_resistance"]]
  lambda_cell_to_pack <- m$parameters[["lambda_cell_to_pack"]]
  lambda_pack_to_ambient <- m$parameters[["lambda_pack_to_ambient"]]
  lambda_pack_AC_to_ambient <- m$parameters[["lambda_pack_AC_to_ambient"]]
  lambda_cooling_power <- m$parameters[["lambda_cooling_power"]]
  COP <- m$parameters[["COP"]]
  heat_capacity <- m$parameters[["heat_capacity"]]

  cat(paste0("predict_temp: r = ", effective_pack_resistance,
      ", λ1 = ", lambda_cell_to_pack,
      ", λ2 = ", lambda_pack_to_ambient,
      ", λ3 = ", lambda_pack_AC_to_ambient,
      ", λ4 = ", lambda_cooling_power,
      ", COP = ", COP,
      "\n"))

  # n.b. additional secondary parameters may be stored in m$parameters

  if (!"delta_t" %in% names(logtibble)) {
    # a minor optimisation: we avoid recomputing these columns

    #compute delta_t for runs of near-consecutive samples
    logtibble <- logtibble |>
      mutate(delta_t = date_time - dplyr::lag(date_time))

    sampling_interval <- as.double(median(logtibble$delta_t, na.rm = TRUE))
    # multiple missing samples will terminate a predictive segment
    # n.b. isolated missing samples do not hugely affect our model's predictions
    # time-stamps in the logs have a precision of 1 second
    max_delta_t <- 2 * sampling_interval + 2
    logtibble <- logtibble |>
      mutate(delta_t = ifelse(delta_t > max_delta_t, NA, delta_t))

    # strangely, pack_t3_c is uniformly NA in all my logfiles.
    logtibble <- logtibble |>
      mutate(pack_avg_temp = rowMeans(across(c(
        pack_t1_c, pack_t2_c, pack_t4_c
      ))), .before = cp1)

    # n.b. ambient temps are unreliable at the start of a segment: they're
    # sometimes reported in degrees F rather than degrees C.
    logtibble <- logtibble |>
      mutate(ambient = ifelse(is.na(delta_t), NA, ambient))

    # n.b. pack temps seem to be reliably reported in degrees C, but we'll
    # monitor this...
    outlier_temps <- which(logtibble$pack_t4_c > 50)
    if (length(outlier_temps) > 0) {
      warning(paste(length(outlier_temps) > 0),
              "temperatures greater than 50 in the pack_t4_c column")
    }

    # rate of heat gain (in K/s)
    logtibble <- logtibble |>
      mutate(delta_K_delta_t =
               (pack_avg_temp - dplyr::lag(pack_avg_temp)) / delta_t,
             .before = cp1)

  }

  # we now predict temperatures, using the parameters

  # predicted Joule heating of cells (in W)
  # n.b. the resistance is in mOhms
  # n.b. variations in pack_amps have a nonlinear effect on heating
  # An estimated slope $m$ in pack_amps, when integrated across the unit
  # interval, adds $m^2 / 2$ to the estimated Joule heating.  We estimate
  # this slope using a 2-point backward divided difference.
  # We also compute a second-order divided difference, to investigate its
  # correlation with the prediction error in our model
  multilag <- function(x, lags = 1:2) {
    names(lags) <- as.character(lags)
    purrr::map_dfr(lags, lag, x = x)
  }
  logtibble <- logtibble |>
    mutate(
      across(pack_amps, multilag, .unpack = TRUE),
      .before = cp1
    ) |>
    rowwise() |>
    mutate(
      slope_amps = (pack_amps - pack_amps_1) / 2,
      acc_amps = (pack_amps - (2 * pack_amps_1) + pack_amps_2) / 2,
      .before = cp1
    ) |>
    ungroup()
  logtibble <- logtibble |>
    mutate(slope_amps = ifelse(is.na(slope_amps) | is.na(delta_t),
                               0, slope_amps),
           acc_amps = ifelse(is.na(acc_amps) | is.na(delta_t),
                             0, acc_amps)
    )
  logtibble <- logtibble |>
    mutate(
      pred_Joule_heating =
        (pack_amps * pack_amps + 0.5 * slope_amps * slope_amps) *
        effective_pack_resistance /
        (soh / 100) /
        1000,
      .before = cp1
    )

  # we now recompute the sampling_interval with more precision, by using the
  # mean rather than the median as we had done when defining segments in the
  # logfile pre-processing step. This calculation is insensitive to the long
  # intervals between segments, and is likely is at a higher precision than the
  # 1-second resolution of timestamps as recorded in the logfile)
  sampling_interval <- as.double(mean(logtibble$delta_t, na.rm = TRUE))

  # unlagged predicted per-sample delta-heating of pack (in temperature K),
  # based on our roughly-estimated heat_capacity. If this parameter is
  # modified, then the best-fit value of effective_pack_resistance is affected
  # (precisely in inverse proportion), since it is the ratio between these two
  # parameters which is the constant of proportionality between the square of
  # pack amperage and its "Joule heating" in units of K/s.
  logtibble <- logtibble |>
    mutate(
      pred_heating_unlagged =
        pred_Joule_heating / heat_capacity * sampling_interval,
      .before = cp1
    )

  # unlagged pack-cooling wattage, assuming that any A/C power is devoted
  # to pack cooling if charge_mode==1, or if the pack is above 40 degrees C.
  # The latter condition is derived from a single observation of A/C running
  # on a cold night while driving with a hot pack (ac24kWh_2024 on Feb 21),
  # so the temperature threshold may be incorrect.
  logtibble <- logtibble |>
    mutate(
      # these are delta-temperatures of cooling, must be summed
      cooling_heatpump_unlagged =
        if_else(charge_mode == 0 & pack_avg_temp < 40,
                0,
                COP * 50 * est_pwr_a_c_50w / heat_capacity * sampling_interval
        ),
      .before = cp1
    )


  # we use an Exponential Moving Average filter on a per-sample basis: rather
  # inaccurate when there are missing samples; but can be efficiently computed
  # on any modern (deeply-pipelined) CPU.  Furthermore, it is undistorted by the
  # roundoff errors in the time-stamps (at 1-second precision!) on the samples
  EMA_parameter_cell_to_pack <- min(1.0,
                                    sampling_interval / lambda_cell_to_pack)
  EMA_parameter_cooling_power <- min(1.0,
                                     sampling_interval / lambda_cooling_power)

  # we now lurch from the tidyverse into the wilds of xts. The EMA is a simple
  # recursive filter; but R's runtime is hostile to recursion.  It's possible to
  # use sapply() to implement a reasonably-efficient tail-recursion, but its
  # correctness relies on an undocumented expectation of sapply: that it
  # performs a sequential, in-order execution of its FUN, and not a vectorised
  # implementation (which would read all of its inputs fully, rather than
  # stalling the computation of the second element of its output until the
  # computation of its first element has completed) See stackoverflow.com/
  # questions/49348870/tibbletime-previous-days-close/49373709#
  # comment140995212_49373709 and github.com/tidyverse/dbplyr/issues/1108

  # predicted delta-heating of pack (in K), with exponential lag, grouped
  # by the gaps in the sampling
  w <- which(is.na(logtibble$delta_t))
  nsegments <- length(w)
  wstart <- w
  wend <- lead(w) - 1
  wend[nsegments] <- length(logtibble$delta_t)
  wexclude <- (wend - wstart) < min_segment_length

  unlagged_heat <- as.xts(logtibble$pred_heating_unlagged, logtibble$date_time)
  lagged_heat <- as.xts(vector(mode = "double",
                               length = length(unlagged_heat)),
                        logtibble$date_time)
  unlagged_cooling <- as.xts(logtibble$cooling_heatpump_unlagged,
                             logtibble$date_time)
  lagged_cooling <- as.xts(vector(mode = "double",
                                  length = length(unlagged_heat)),
                           logtibble$date_time)

  for (i in seq(nsegments)[which(!wexclude)]) {
    lagged_heat[wstart[i]:wend[i]] <-
      stats::filter(
        unlagged_heat[wstart[i]:wend[i]] * EMA_parameter_cell_to_pack,
        1. - EMA_parameter_cell_to_pack,
        method = "recursive",
        init = 0.
      )
    lagged_cooling[wstart[i]:wend[i]] <-
      stats::filter(
        unlagged_cooling[wstart[i]:wend[i]] * EMA_parameter_cooling_power,
        1. - EMA_parameter_cooling_power,
        method = "recursive",
        init = 0.
      )
  }

  pack_avg_temp_xts <-
    xts(logtibble$pack_avg_temp, logtibble$date_time)

  # predict pack temps from cumsum of lagged delta-heat in each segment
  segment_starts <- logtibble$date_time[wstart]
  pred_pack_avg_temp_xts <- stats::lag(lagged_heat)

  pred_pack_avg_temp_xts[wstart] <- pack_avg_temp_xts[wstart] # observations
  for (i in seq(nsegments)[which(!wexclude)]) {
    pred_pack_avg_temp_xts[wstart[i]:wend[i]] <-
      cumsum(pred_pack_avg_temp_xts[wstart[i]:wend[i]]) # predictions
  }

  # we now revert to base R

  # decrease pack temps if there's any active cooling, with a lag
  # to model the delay before the refrigerant becomes cold at the
  # heat-exchanger inside the pack
  # n.b. due to the lag, there may be some cooling energy that isn't
  # summed into the interval.  We evaluate this with a cumsum of the
  # unlagged cooling over the interval.  There will also be some cooling
  # energy reaching the pack from A/C operations prior to the start of
  # an interval, but that could only be roughly estimated and we don't
  # attempt this.

  cumsum_cooling <- vector(mode = "double",
                                  length = length(lagged_cooling))
  cumsum_cooling_unlagged <- vector(mode = "double",
                                  length = length(unlagged_cooling))

  for (i in seq(nsegments)[which(!wexclude)]) {
    cumsum_cooling[wstart[i]:wend[i]] <-
      cumsum(lagged_cooling[wstart[i]:wend[i]])
    cumsum_cooling_unlagged[wstart[i]:wend[i]] <-
      cumsum(unlagged_cooling[wstart[i]:wend[i]])
  }
  # preserve the cumsums of cooling in logtibble, for ease in debugging
  logtibble <- logtibble |>
    mutate(
      cooling = cumsum_cooling,
      cooling_diff = cumsum_cooling_unlagged - cumsum_cooling,
      .before = cp1
    )
  pred_temp_v <- as.vector(pred_pack_avg_temp_xts[,1,drop=TRUE])

  pred_temp_v <- pred_temp_v - cumsum_cooling

  # Cool the pack by its convection to the ambient air
  #
  # TODO: determine if the pack_to_ambient time constant should decrease with
  # the vehicle's velocity

  # N.b. the computational kernel of this filter could be executed efficiently
  # on any superscalar CPU, but is an inefficient scalar-mode computation
  # when expressed in the vector-Fortran computational model of R.

  # n.b. the second and third time constants are in hours
  EMA_parameter_pack_to_ambient <- min(1.0,
    sampling_interval / (lambda_pack_to_ambient * 3600))
  EMA_parameter_pack_AC_to_ambient <- min(1.0,
    sampling_interval / (lambda_pack_AC_to_ambient * 3600))
  EMA_parameter_pack_cooling <- ifelse(
    # on 2024-02-21, a hot pack was being actively cooled while driving!
    # we assume this was due to pack temp > 40
    ((logtibble$charge_mode == 0) & (pred_temp_v < 40) |
      (logtibble$est_pwr_a_c_50w == 0)),
                          EMA_parameter_pack_to_ambient,
                          EMA_parameter_pack_AC_to_ambient)
  ambient_v <- logtibble$ambient
  old_pred_v <- pred_temp_v
  for (i in seq(nsegments)[which(!wexclude)]) {
    # It might be interesting to benchmark R's Tailcall() against the following
    # "manual" TCO of a simple recursive filter.  See
    # stackoverflow.com/questions/78979492/optimization-of-tail-recursion-in-r
    prev_newpred <- pred_temp_v[wstart[i]] # base case
    prev_oldpred <- prev_newpred
    for (j in seq(wstart[i] + 1, wend[i])) {  # a scalar inner loop, ouch!
      curr_oldpred <- old_pred_v[j]
      # new delta-temp is the old delta-temp plus a small shift toward ambient
      curr_newpred <- prev_newpred +
        (curr_oldpred - prev_oldpred) +
        (ambient_v[j] - prev_newpred) * EMA_parameter_pack_cooling[j]
      pred_temp_v[j] <- curr_newpred
      prev_oldpred <- curr_oldpred
      prev_newpred <- curr_newpred
    }
  }

  # we make no predictions for short sequences
  pred_temp_v[wstart] <- NA
  for (i in seq(nsegments)[which(wexclude)]) {
    pred_temp_v[wstart[i]:wend[i]] <- NA
  }

  # return to the tidyverse!  Hooray!!
  logtibble <- logtibble |>
    mutate(pred_pack_avg_temp = pred_temp_v,
           err_pred = pred_pack_avg_temp - pack_avg_temp,
           .before = "cp1") |>
    select(!c(pred_Joule_heating, pred_heating_unlagged))
  m$logdata <- logtibble
  m$modified.last.time <- now()

  maxpe <- which.max(logtibble$err_pred)
  minpe <- which.min(logtibble$err_pred)
  cat("Underprediction by",
      round(logtibble$err_pred[minpe], 2),
      "degrees at",
      format_ISO8601(logtibble$date_time[minpe]),
      "\n")
  cat("Overprediction by",
      round(logtibble$err_pred[maxpe], 2),
      "degrees at",
      format_ISO8601(logtibble$date_time[maxpe]),
      "\n")

  return(m)

}
