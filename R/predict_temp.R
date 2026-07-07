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
#' As a round number, the heat capacity of the pack is thus 1.0e6 J/K.  This is a
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
#' @param effective_pack_resistance in mOhms at 298.15K, a primary parameter
#' @param lambda_cell_to_pack in seconds, a primary parameter
#' @param lambda_pack_to_ambient in hours, a primary parameter
#' @param lambda_pack_AC_to_ambient in hours, a primary parameter
#' @param fan_power in Watts, a primary parameter
#' @param COP dimensionless, a primary parameter
#' @param arrhenius_resistance in K, a primary parameter
#' @param heat_capacity in J/K, a secondary parameter
#' @param iter_count controls convergence on predicted temps
#' @param min_segment_length shorter sequences of samples are ignored
#' @param logfilnm name of a csv logfile to be read, if is.null(tmodel)
#' @param logfildir directory in which the logfile is located
#' @param from_date starting date/time for calculation of MSE
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
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
                         fan_power = NA,
                         COP = NA,
                         arrhenius_resistance = NA,
                         heat_capacity = NA,
                         iter_count = 4,
                         min_segment_length = 10,
                         logfilnm = "log26Jan2026.csv",
                         logfildir = "data-raw",
                         from_date = NULL,
                         to_date = NULL,
                         from_idx = NULL,
                         to_idx = NULL) {

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
  if (!is.na(fan_power)) {
    m$parameters[["fan_power"]] <- fan_power
  }
  if (!is.na(COP)) {
    m$parameters[["COP"]] <- COP
  }
  if (!is.na(arrhenius_resistance)) {
    m$parameters[["arrhenius_resistance"]] <- arrhenius_resistance
  }
  if (!is.na(heat_capacity)) {
    m$parameters[["heat_capacity"]] <- heat_capacity
  }

  # read a full set of params
  effective_pack_resistance <- m$parameters[["effective_pack_resistance"]]
  lambda_cell_to_pack <- m$parameters[["lambda_cell_to_pack"]]
  lambda_pack_to_ambient <- m$parameters[["lambda_pack_to_ambient"]]
  lambda_pack_AC_to_ambient <- m$parameters[["lambda_pack_AC_to_ambient"]]
  fan_power <- m$parameters[["fan_power"]]
  COP <- m$parameters[["COP"]]
  arrhenius_resistance <- m$parameters[["arrhenius_resistance"]]
  heat_capacity <- m$parameters[["heat_capacity"]]

  cat(paste0("predict_temp: r = ", effective_pack_resistance,
             ", λ1 = ", lambda_cell_to_pack,
             ", λ2 = ", lambda_pack_to_ambient,
             ", λ3 = ", lambda_pack_AC_to_ambient,
             ", fanp = ", fan_power,
             ", COP = ", COP,
             ", a = ", arrhenius_resistance,
             "\n"))
  # n.b. additional secondary parameters may be stored in m$parameters

  if (COP < 0.5) {
    COP <- 0.5
    warning("The minimum COP of a fit is 0.5\n")
  }

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

  # we perform an iterative approximation to the predicted temperatures, because of
  # the significant shifts in effective pack resistance as a function of
  # temperature. There are also some shifts in the vehicle's estimated %Hx, so
  # we work from its value at the beginning of each prediction segment. Possibly
  # someone will someday backsolve the updating processes for estimating %Hx.

  w <- which(is.na(logtibble$delta_t))
  nsegments <- length(w)
  wstart <- w
  wend <- lead(w) - 1
  wend[nsegments] <- length(logtibble$delta_t)
  wexclude <- (wend - wstart) < min_segment_length

  segnumv = rep(0, length(logtibble$delta_t))
  for (i in seq(nsegments)[which(!wexclude)]) {
    segnumv[wstart[i]:wend[i]] = i
  }

  #n.b. we don't predict in segnum == 0
  logtibble <- logtibble |>
    mutate(segnum = segnumv, .before = pack_avg_temp) |>
    group_by(segnum) |>
    mutate(pred_pack_avg_temp =
             if_else(segnum == 0, NA, first(pack_avg_temp)),
           pred_hx =
             if_else(segnum == 0, NA, first(hx)),
           .before = pack_avg_temp) |>
    ungroup()

  # we recompute the sampling_interval with more precision, by using the
  # mean rather than the median as we had done when defining segments in the
  # logfile pre-processing step. This calculation is insensitive to the long
  # intervals between segments, and is likely is at a higher precision than the
  # 1-second resolution of timestamps as recorded in the logfile)
  #
  sampling_interval <- as.double(mean(logtibble$delta_t, na.rm = TRUE))

  # Sanity-test the sampling_interval, just in case someone had modified this
  # aspect of the LeafSpy setup within the span of a thmodel. One possibility is
  # to confirm that sampling_interval is within the 49th to 51st percentile of
  # the delta_t.  Here we merely confirm it is within 1 second of the median of
  # the delta_t.  There may be some skew from missing samples due to an
  # overloaded phone.
  if( abs(sampling_interval -
      as.double(median(logtibble$delta_t, na.rm = TRUE))) >= 1) {
    warning("Sampling interval may have been changed on LeafSpy\n")
  }

  for (iternum in 1:iter_count) {

    # if iternum>1, we're using the previous prediction of temperature
    # to estimate the effective pack resistance (after the first sample)
    #
    # if iternum==1, we use the pack temperature at the beginning of a
    # segment to estimate the effective pack resistance for the whole of
    # the segment
    for (i in seq(nsegments)[which(!wexclude)]) {
      logtibble$pred_pack_avg_temp[wstart[i]] <-
        logtibble$pack_avg_temp[wstart[i]]
    }
    logtibble <- logtibble |>
      mutate(
        eff_packr = effective_pack_resistance *
          exp(arrhenius_resistance *
                (1 / 298.15 - 1 / (pred_pack_avg_temp + 273.15))) /
          (pred_hx / 100),
        pred_Joule_heating =
          (pack_amps * pack_amps + 0.5 * slope_amps * slope_amps) *
          eff_packr /
          1000, # in kW
        .before = cp1
      )

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

    # unlagged pack-cooling wattage, assuming that if charge_mode==1, then
    # any A/C power above fan_power is running the A/C compressor and its
    # refrigerant is being circulated through the pack's evaporator
    logtibble <- logtibble |>
      mutate(
        cooling_power = 50 * est_pwr_a_c_50w - fan_power,
        cooling_power = if_else(cooling_power < 0, 0, cooling_power),
        # these are delta-temperatures of cooling, must be summed
        cooling_heatpump_unlagged =
          if_else(
            charge_mode == 0,
            0,
            COP * cooling_power / heat_capacity * sampling_interval
          ),
        .before = cp1
      )

    # we use an Exponential Moving Average filter on a per-sample basis: rather
    # inaccurate when there are missing samples; but can be efficiently computed
    # on any modern (deeply-pipelined) CPU.  Furthermore, it is undistorted by the
    # roundoff errors in the time-stamps (at 1-second precision!) on the samples
    EMA_parameter_cell_to_pack <- min(1.0,
                                      sampling_interval / lambda_cell_to_pack)

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

    unlagged_heat <- as.xts(logtibble$pred_heating_unlagged, logtibble$date_time)
    lagged_heat <- as.xts(vector(mode = "double",
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
    }

    # we now revert to base R
    pack_deltas_v <- as.vector(lagged_heat[,1,drop=TRUE]) -
      logtibble$cooling_heatpump_unlagged

    # predict pack temps from the observations at wstart, plus the cumsum of
    # lagged delta-heat in each segment
    pred_pack_avg_temp_v <- logtibble$pack_avg_temp
    for (i in seq(nsegments)[which(!wexclude)]) {
      # predictions at !wstart
      obsi <- pred_pack_avg_temp_v[wstart[i]]
      deltasi <- pack_deltas_v[(wstart[i] + 1) : wend[i]]
      pred_pack_avg_temp_v[(wstart[i] + 1) : wend[i]] <-
        rep(obsi, (wend[i] - wstart[i])) + cumsum(deltasi)
    }

    # Cool (or warm!) the pack via convection to the ambient air
    #
    # TODO: determine if the pack_to_ambient time constant should decrease with
    # the vehicle's velocity
    #
    # N.b. the computational kernel of this filter could be executed efficiently
    # if compiled for a superscalar CPU, but must be expressed as an inefficient
    # scalar-mode computation in R's variant of vector-Fortran.

    # n.b. these time constants are in hours
    EMA_parameter_pack_to_ambient <- min(1.0,
                                         sampling_interval / (lambda_pack_to_ambient * 3600))
    EMA_parameter_pack_AC_to_ambient <- min(1.0,
                                            sampling_interval / (lambda_pack_AC_to_ambient * 3600))
    # n.b. when the fan is running inside the pack, the module-to-pack
    # thermal conductivity is significantly increased -- so the time constant
    # is significantly shorter on the module-to-ambient equilibrium
    # n.b. these time constants are misleadingly named, as the thermosensors
    # are on the modules rather than on the shell of the pack
    EMA_parameter_pack_cooling <- ifelse(
      ((logtibble$charge_mode == 0) |
         (logtibble$est_pwr_a_c_50w == 0)),
      EMA_parameter_pack_to_ambient,
      EMA_parameter_pack_AC_to_ambient)

    # the following is a manual TCO of the recursive filter
    # $x_t = x_{t-1} + d_t + (a_t - x_{t-1} - d_t)\lambda$
    # where $x_t$ is the pack temperature, $d_t$ is the heatflow from the cells,
    # $a_t$ is the ambient temperature, and $\lambda$ is the time constant
    # for heatflow from pack to ambient.

    pred_temp_v <- pred_pack_avg_temp_v
    pred_deltat_v <- pred_temp_v - lag(pred_temp_v, default = NULL)
    # we hoist two vector-mode computations from the loop
    ambient_heat_v <- logtibble$ambient * EMA_parameter_pack_cooling
    EMA1 <- 1.0 - EMA_parameter_pack_cooling

    for (i in seq(nsegments)[which(!wexclude)]) {
      prevpred <- pred_temp_v[wstart[i]]
      for (j in seq(wstart[i] + 1, wend[i])) {  # a scalar inner loop, ouch!
        nextpred <- (prevpred + pred_deltat_v[j]) * EMA1[j] + ambient_heat_v[j]
        pred_temp_v[j] <- nextpred
        prevpred <- nextpred  # loop-carried dependency
      }
    }

    # we avoid skewing statistics of fit
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

    # curiously, xts insists on UTC for stored dates & times
    from_idx <- ifelse(is.null(from_date),
                       ifelse(is.null(from_idx), 1, from_idx),
                       dplyr::first(which(
                         logtibble$date_time >= as.POSIXct(from_date, tz = "UTC")
                       )))
    to_idx <- ifelse(is.null(to_date),
                     ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                     dplyr::last(which(
                       logtibble$date_time <= as.POSIXct(to_date, tz = "UTC")
                     )))
    if (is.na(from_idx) || is.na(to_idx)) {
      warning("Date out of range")
    } else if (from_idx >= to_idx) {
      warning("from_date is not before to_date")
    }

    maxpe <- which.max(logtibble$err_pred)
    maxpew <- which.max(logtibble$err_pred[from_idx:to_idx])
    minpe <- which.min(logtibble$err_pred)
    minpew <- which.min(logtibble$err_pred[from_idx:to_idx])
    if ((to_idx - from_idx + 1) < length(logtibble$err_pred)) {
      cat("Iteration", iternum, ":\n")
      cat("  Underprediction in window by",
          round(logtibble$err_pred[minpew], 2),
          "degrees at",
          format_ISO8601(logtibble$date_time[minpew]),
          "\n")
      cat("  Overprediction in window by",
          round(logtibble$err_pred[maxpew], 2),
          "degrees at",
          format_ISO8601(logtibble$date_time[maxpew]),
          "\n")
    }
    if ((to_idx - from_idx + 1) >= length(logtibble$err_pred)) {
      cat("Iteration", iternum, ":\n")
    }
    cat("  Underprediction in the full dataset by",
        round(logtibble$err_pred[minpe], 2),
        "degrees at",
        format_ISO8601(logtibble$date_time[minpe]),
        "\n")
    cat("  Overprediction in the full dataset by",
        round(logtibble$err_pred[maxpe], 2),
        "degrees at",
        format_ISO8601(logtibble$date_time[maxpe]),
        "\n")

  }

  return(m)

}
