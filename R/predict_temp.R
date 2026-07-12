#' Uses a 7-parameter thermal model to predict temperatures in a LeafSpy log
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
#' @param trace 0 for silent, 1 for minimal, 2 for verbose
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
                         min_segment_length = 20,
                         trace = 2,
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

  if (trace > 0) {
    cat(paste0("predict_temp: r = ", effective_pack_resistance,
             ", λ1 = ", lambda_cell_to_pack,
             ", λ2 = ", lambda_pack_to_ambient,
             ", λ3 = ", lambda_pack_AC_to_ambient,
             ", fanp = ", fan_power,
             ", COP = ", COP,
             ", a = ", arrhenius_resistance,
             "\n"))
  }
  # n.b. additional secondary parameters may be stored in m$parameters

  if (COP < 0.0) {
    COP <- 0.0
    warning("The minimum COP of a fit is 0.0\n")
    # avoids a possible runaway negative COP in optim() if fan_power is low.
    # (It's the effective pack resistance which causes most of the heat gain
    # during a fastcharge.)
  }

  if (!"delta_t" %in% names(logtibble)) {
    # a minor optimisation: we avoid recomputing these columns

    #compute delta_t for runs of near-consecutive samples
    logtibble <- logtibble |>
      mutate(delta_t = date_time - dplyr::lag(date_time))
    # n.b. dplyr's lag/lead is sort-of-intuitive if you imagine lag() as an
    # element-wise operation which retrieves the "previous" value in a vector,
    # rather than imagining that a lagged timeseries has been shifted
    # "backwards" (toward lower-indexed/earlier values).  It's a hazardous
    # semantic conflict with xts::lag() and stats::lag()!

    # we make a rude estimate of the sampling interval over the whole file
    # in order to count missing samples (with reasonable accuracy)
    # TODO: review this code for adequacy on files with a non-constant
    # sampling interval (which may be changed at any time by the LeafSpy user)
    sampling_interval_est <- as.double(median(logtibble$delta_t, na.rm = TRUE))
    # multiple missing samples will terminate a predictive segment
    # n.b. time-stamps in the logs have a precision of 1 second
    max_delta_t <- 2 * sampling_interval_est + 2
    logtibble <- logtibble |>
      mutate(delta_t = ifelse(delta_t > max_delta_t, NA, delta_t))

    # strangely, pack_t3_c is uniformly NA in all my logfiles.
    logtibble <- logtibble |>
      mutate(pack_avg_temp = rowMeans(across(c(
        pack_t1_c, pack_t2_c, pack_t4_c
      ))), .before = cp1)

    # n.b. pack temps are unreliable when LeafSpy is still initialising,
    # as it sometimes stutters on the previous temp readouts.  We rely heavily
    # on the first temp readings in a predictive segment as the basis of
    # our temperature predictions, so must delay starting the prediction
    # until these readouts are stable.
    # see e.g. eNV200ac24kWh_2025 2025-08-31 09:15:06
    logtibble <- logtibble |>
      mutate(
        wonky_temps = !is.na(dplyr::lead(delta_t)) &
          (abs((pack_avg_temp - dplyr::lead(pack_avg_temp))) > 1),
        delta_t = ifelse(wonky_temps, NA, delta_t)
        )
    wwonky <- which(logtibble$wonky_temps)
    if (length(wwonky) > 0) {
      warning(paste("Omitted implausible temperature reading(s) at",
                      paste(format_ISO8601(logtibble$date_time[wwonky]),
                            collapse = " "),
                      collapse = " "))
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
  wend <- dplyr::lead(w) - 1
  wend[nsegments] <- length(logtibble$delta_t)
  wexclude <- (wend - wstart) < min_segment_length

  segnumv = rep(0, length(logtibble$delta_t))
  for (i in seq(nsegments)[which(!wexclude)]) {
    segnumv[wstart[i]:wend[i]] = i
  }

  # the sampling interval is a parameter in LeafSpy which
  # we estimate on a per-segment basis.
  #n.b. we don't predict in segnum == 0
  logtibble <- logtibble |>
    mutate(segnum = segnumv, .before = pack_avg_temp) |>
    group_by(segnum) |>
    mutate(sampling_interval = mean(delta_t, na.rm = T),
           pred_pack_avg_temp =
             if_else(segnum == 0, NA, first(pack_avg_temp)),
           pred_hx =
             if_else(segnum == 0, NA, first(hx)),
           EMA_parameter_cell_to_pack =
             min(1.0, sampling_interval / lambda_cell_to_pack),
           # n.b. the next two time constants are in hours
           EMA_parameter_pack_to_ambient =
             min(1.0, sampling_interval / (lambda_pack_to_ambient * 3600)),
           EMA_parameter_pack_AC_to_ambient =
             min(1.0, sampling_interval / (lambda_pack_AC_to_ambient * 3600)),
           .before = pack_avg_temp) |>
    ungroup()

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

    # we use an Exponential Moving Average filter on a per-sample basis:
    # inaccurate if there are many missing samples -- but easily coded as a
    # recursion, and quite efficient if compiled in C for any modern
    # (deeply-pipelined) CPU.  Furthermore, it is undistorted by the roundoff
    # errors in the time-stamps (at 1-second precision!) on the samples.
    # Annoyingly, the tidyverse has no support for recursive filtering, and the
    # vector-FORTRAN execution model of R is hostile to recursive function
    # calls. Any EMA could be implemented as a tail-recursion in sapply() -- but
    # only if we assume that it will if we assume it will always perform a
    # sequential, in-order execution of its FUN.  An R that's optimised for a
    # vector supercomputer could thus implement an sapply() which fully reads
    # all of its inputs before computing any outputs -- and fail to which would
    # thus fail to implement the loop-carried dependency of an iterative
    # implementation of a tail recursion. See stackoverflow.com/
    # questions/49348870/tibbletime-previous-days-close/49373709#
    # comment140995212_49373709 and github.com/tidyverse/dbplyr/issues/1108

    # We predict the delta-heating as measured at the thermosensors (in K), with
    # exponential lag, grouped by the gaps in the sampling.  The lag is much
    # more significant in the 50kWh pack, perhaps because there are only four
    # modules (with thermosensors mounted on their exterior), whereas in the
    # original-equipment 24kWh pack, there are 48 modules of which four have
    # thermosensors (which -- apparently -- are in good thermal contact with the
    # cells inside)
    unlagged_heat <- as.xts(logtibble$pred_heating_unlagged, logtibble$date_time)
    lagged_heat <- as.xts(vector(mode = "double",
                                 length = length(unlagged_heat)),
                          logtibble$date_time)
    for (i in seq(nsegments)[which(!wexclude)]) {
      EMA_param_1 <- logtibble$EMA_parameter_cell_to_pack[wstart[1]]
      lagged_heat[wstart[i]:wend[i]] <-
        stats::filter(
          unlagged_heat[wstart[i]:wend[i]] * EMA_param_1,
          1. - EMA_param_1,
          method = "recursive",
          init = 0.
        )
    }

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
            COP * cooling_power / heat_capacity * delta_t
          ),
        .before = cp1
      )

    # we now revert to base R, to implement an EMA that is outside the scope of
    # stats:filter()
    #
    # n.b. when the fan is running inside the pack, the module-to-pack thermal
    # conductivity is significantly increased -- so the time constant is
    # significantly shorter on the module-to-ambient equilibrium
    #
    # TODO: determine if the pack_to_ambient time constant should decrease with
    # the vehicle's velocity
    #
    # We hoist the following computations from our scalar inner loop.
    EMA_param_23 <- ifelse(
      ((logtibble$charge_mode == 0) |
         (logtibble$est_pwr_a_c_50w == 0)),
      logtibble$EMA_parameter_pack_to_ambient, # \lambda_2
      logtibble$EMA_parameter_pack_AC_to_ambient # \lambda_3
    )
    EMA_param_23_complement <- 1.0 - EMA_param_23

    # the following is a manual TCO of the recursive filter $x_t =
    # x_{t-1}(1-\lambda) + h_t + a_t \lambda$ where $x_t$ is the pack
    # temperature, $h_t$ is the heatflow into the cells (in K), $a_t$ is the
    # ambient temperature, and $\lambda$ is the time constant for heatflow from
    # pack to ambient (in units of the sampling_interval, rather than seconds or
    # hours)
    pred_temp_v <- logtibble$pack_avg_temp
    heat_in_v <- as.vector(lagged_heat[,1,drop=TRUE]) -
      logtibble$cooling_heatpump_unlagged
    ambient_v <- logtibble$ambient
    for (i in seq(nsegments)[which(!wexclude)]) {
      prevpred <- pred_temp_v[wstart[i]]
      for (j in seq(wstart[i] + 1, wend[i])) {  # a scalar inner loop, ouch!
          nextpred <- prevpred * EMA_param_23_complement[j] +
          heat_in_v[j] +
          ambient_v[j] * EMA_param_23[j]
        pred_temp_v[j] <- nextpred
        prevpred <- nextpred  # loop-carried dependency
      }
    }

    # we mask unpredicted temps with NA, to avoid skewing statistics of fit
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
    if ((trace > 1) &&
        ((to_idx - from_idx + 1) < length(logtibble$err_pred))) {
      cat("Iteration",
          iternum,
          ": Prediction error in window: (",
          round(logtibble$err_pred[minpew], 2),
          ",",
          round(logtibble$err_pred[maxpew], 2),
          ")\n")
      if (iternum == iter_count) {
        cat("    at (",
            format_ISO8601(logtibble$date_time[minpew]),
            ",",
            format_ISO8601(logtibble$date_time[maxpew]),
            ")\n")
      }
    }
    if (trace > 1) {
      cat("Iteration",
          iternum,
          ": Prediction error in the full dataset: (",
          round(logtibble$err_pred[minpe], 2),
          ",",
          round(logtibble$err_pred[maxpe], 2),
          ")\n"
      )
      if (iternum == iter_count) {
        cat("    at (",
            format_ISO8601(logtibble$date_time[minpe]),
            ",",
            format_ISO8601(logtibble$date_time[maxpe]),
            ")\n"
        )
      }
    }
  }

  return(m)

}
