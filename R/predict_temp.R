#' Evaluates a thermal model to predict temperatures in a LeafSpy log
#'
#' A LeafSpy logfile may be specified to this function by filename and
#' directory, in which case this logfile is munged -- to mitigate the privacy
#' risk of publishing a VIN, and to revise column names so that they're tidy.
#' This routine also does some "cleaning" of obviously-wonky data e.g. of a
#' pack at 0 Volts or at 80 degrees (in a field that is normally in Centrigade
#' units).
#'
#' The output of this routine is a thmodel object containing the munged and
#' cleaned data from LeafSpy augmented with additional columns for the
#' predictions of pack temperature (and for convenience when plotting). This
#' thmodel records a timestamp of its modification, and the values of the
#' modelling parameters used for its updated temperature predictions.
#'
#' Initial estimates of parameters are hardcoded in default_params().
#'
#' @param tmodel a thmodel, optional
#' @param effective_pack_resistance in mOhms at 298.15K for SOC <= 70 percent
#' @param packr85 in mOhms, effective pack resistance at SOC >= 85 percent
#' @param polarisation_energy in kJ/V, a reversible (entropic) heat
#' @param lambda_module_to_ambient in hours
#' @param lambda_module_AC_to_ambient in hours
#' @param fan_power in Watts
#' @param COP dimensionless
#' @param arrhenius_resistance in K, temperature dependence of effective packr
#' @param heat_capacity in kJ/K
#' @param ocv_tbl maps SOC onto OCV, either a 2-column tibble or an om_model
#' @param iter_count may be increased for a more accurate prediction
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
#' m <- predict_temp() # default dataset is data-raw/log26Jan26.csv
#' m <- m |> predict_temp(effective_pack_resistance = 67)

predict_temp <- function(tmodel = NULL,
                         effective_pack_resistance = NA,
                         packr85 = NA,
                         polarisation_energy = NA,
                         lambda_module_to_ambient = NA,
                         lambda_module_AC_to_ambient = NA,
                         fan_power = NA,
                         COP = NA,
                         arrhenius_resistance = NA,
                         heat_capacity = NA,
                         ocv_tbl = NULL,
                         iter_count = 4,
                         min_segment_length = 20,
                         trace = 2,
                         logfilnm = "log26Jan2026.csv",
                         logfildir = "data-raw",
                         from_date = NULL,
                         to_date = NULL,
                         from_idx = NULL,
                         to_idx = NULL) {

  #' Notes on heat capacity:
  #'
  #' The Joule heating (in K) of a pack is the square of its amperage, multiplied
  #' by its effective resistance and divided by its heat capacity (in J/K).
  #' An accurate estimation of the effective resistance for a pack, based on its
  #' observed thermal behaviour, is possible only with an accurate estimation
  #' of its heat capacity.
  #'
  #' I recommend the thermal modelling of predict_temp(), with some judicious use
  #' of fit_model(), be used to refine an initial estimate of heat capacity and
  #' all other non-resistive parameters in my model, based on a fixed estimate of
  #' the pack's effective resistance. With these refined estimates, the pack
  #' voltage modelling of predict_volts() method can be used to refine the
  #' resistance model.  Sourcing om_eNV24kWh.R and om_eNV50kWh.R will produce two
  #' ocv_models (one for each size of pack), as required for the use of est_ocv()
  #' and the optimisation routine fit_r_to_ocv().  These methods return a refined
  #' ocv_model, notably including a revised ocv_table that maps the pack's SOC (as
  #' reported by LeafSpy) onto the pack's (estimated) open circuit voltage (OCV).
  #' The fit_r_to_ocv() method searches for better-fitting resistances to the
  #' observed changes in pack voltage as a function of pack amperage, pack
  #' temperature, SOC, and Hx. The fit_r_to_ocv() method also adjusts the
  #' heat_capacity parameter so that the total Joule heating over the dataset
  #' remains constant -- because (except for the reversible entropic heating and
  #' the rather slow cooling processes) the change in pack temperature is
  #' proportional to the rate of Joule heating divided by its heat_capacity.

  if (!nzchar(logfilnm) && is.null(tmodel)) {
    stop("Aborting. Please specify the name of a LeafSpy logfile.")
  }
  else {
    m <- tmodel
    if (is.null(m) || m$name == "") {
      m <- munge_logfile(logfilnm = logfilnm, logfildir = logfildir)
      if (logfilnm == "log26Jan2026.csv") {
        m$model <- "e-NV200" # required for default_params(m)
        m$capacity <- 50
      }
    }
  }

  if (length(m$parameters) == 0) {
    m <- default_params(m)
  }
  logtibble <- m$logdata

  # param values specified in the method call have precedence.
  if (!is.null(ocv_tbl)) {
    if (class(ocv_tbl) == "ocv_model") {
      om <- ocv_tbl
      # om's parameters will be "pasted into" the thmodel if their values were not
      # specified in the call to predict_temp()
      if (is.na(effective_pack_resistance)) {
        effective_pack_resistance <- om$parameters[["effective_pack_resistance"]]
      }
      if (is.na(packr85)) {
        packr85 <- om$parameters[["packr85"]]
      }
      if (is.na(heat_capacity)) {
        heat_capacity <- om$parameters[["heat_capacity"]]
      }
      ocv_tbl <- om$ocv_tbl  # extract the ocv_tbl from this ocv_model
    }
    stopifnot(dim(ocv_tbl)[2] == 2) # sanity checks
    stopifnot(min(ocv_tbl[1]) >= 0.0)
    stopifnot(max(ocv_tbl[1]) <= 1.0)
    m$parameters[["ocv_tbl"]] <- ocv_tbl # update the thmodel's params
  }
  if (!is.na(effective_pack_resistance)) {
    m$parameters[["effective_pack_resistance"]] <- effective_pack_resistance
  }
  if (!is.na(packr85)) {
    m$parameters[["packr85"]] <- packr85
  }
  if (!is.na(polarisation_energy)) {
    m$parameters[["polarisation_energy"]] <- polarisation_energy
  }
  if (!is.na(lambda_module_to_ambient)) {
    m$parameters[["lambda_module_to_ambient"]] <- lambda_module_to_ambient
  }
  if (!is.na(lambda_module_AC_to_ambient)) {
    m$parameters[["lambda_module_AC_to_ambient"]] <- lambda_module_AC_to_ambient
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
  packr85 <- m$parameters[["packr85"]]
  polarisation_energy <- m$parameters[["polarisation_energy"]]
  lambda_module_to_ambient <- m$parameters[["lambda_module_to_ambient"]]
  lambda_module_AC_to_ambient <- m$parameters[["lambda_module_AC_to_ambient"]]
  fan_power <- m$parameters[["fan_power"]]
  COP <- m$parameters[["COP"]]
  arrhenius_resistance <- m$parameters[["arrhenius_resistance"]]
  heat_capacity <- m$parameters[["heat_capacity"]]

  if (trace > 0) {
    cat(paste0("predict_temp:",
               " a = ", round(arrhenius_resistance, 5),
               ", c = ", round(heat_capacity, 5),
               ", pe = ", round(polarisation_energy, 5),
               ", λp = ", round(lambda_module_to_ambient, 5),
               ", λa = ", round(lambda_module_AC_to_ambient, 5),
               ", fanp = ", round(fan_power, 5),
               ", COP = ", round(COP, 5),
               ", r = ", round(effective_pack_resistance, 5),
               ", r85 = ", round(packr85, 5),
               "; "))
  }
  if (trace > 1) {
    print(ocv_tbl)
  }

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
  # interval, adds $m^2 / 2$ to the estimated Joule heating.  We estimate this
  # slope using a 2-point backward divided difference.

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

  # we perform an iterative approximation to the predicted temperatures, because
  # of the significant shifts in effective pack resistance as a function of
  # temperature. There are also some shifts in the vehicle's estimated %Hx, and
  # we work from its value at the beginning of each prediction segment. The
  # process which updates estimates of %Hx is obscure, but could presumably be
  # black-box reverse-engineered with the aid of a simulation such as this one.

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
           # n.b. these time constants are in hours
           EMA_parameter_module_to_ambient =
             min(1.0, sampling_interval / (lambda_module_to_ambient * 3600)),
           EMA_parameter_module_AC_to_ambient =
             min(1.0, sampling_interval / (lambda_module_AC_to_ambient * 3600)),
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

    sloper <- (packr85 - effective_pack_resistance) / 15
    f_soc_to_v <- approxfun(m$parameters[["ocv_tbl"]],
                            method = "linear",
                            rule = 2)
    logtibble <- logtibble |>
      group_by(segnum) |>
      mutate(
        ssoc = soc / 1e6, # scale to (0.0, 1.0)
        # pack is modelled as having a constant resistance for soc in (0%, 70%);
        # then linearly increasing to packr85 at soc = 85%; then constant at
        # packr85 for soc >= 85%
        eff_packr =
          ifelse(
            ssoc <= 0.70,
            effective_pack_resistance,
            ifelse(
              ssoc >= 0.85,
              packr85,
              effective_pack_resistance + sloper * (ssoc - 0.70)
            )
          ) * exp(arrhenius_resistance *
                    (1 / 298.15 - 1 / (pred_pack_avg_temp + 273.15))) /
          (pred_hx / 100),
        pred_pack_volts = f_soc_to_v(ssoc) - pack_amps * eff_packr / 1000,
        pred_Joule_heating =
          (pack_amps * pack_amps + 0.5 * slope_amps * slope_amps) *
          eff_packr / 1000 * delta_t, # in Ws.  Note: r is in mOhms.
        delta_v = pack_volts -
          dplyr::lag(pack_volts, default = first(pack_volts)),
        pred_polarisation_heating =
          delta_v * polarisation_energy * 1000, # in Ws. Reversible.
        cooling_power = 50 * est_pwr_a_c_50w - fan_power,
        cooling_power = if_else(cooling_power < 0, 0, cooling_power),
        heat_pump_cooling = if_else(
          charge_mode == 0,
          0, # AC is cooling the cabin
          COP * cooling_power * delta_t # AC is cooling the battery
        ),
        # predict per-sample delta-heating of pack (in temperature K)
        # n.b. heat_capacity is in kJ/K
        pred_heating = (pred_Joule_heating +
                          pred_polarisation_heating -
                          heat_pump_cooling
                        ) / (heat_capacity * 1000),
        .before = cp1
      ) |>
      ungroup()

    # we now revert to base R, to implement an exponential moving average
    # filter that is outside the scope of stats:filter()
    #
    # n.b. when the fan is running inside the pack, the module-to-pack thermal
    # conductivity is significantly increased -- so the time constant is
    # significantly shorter on the module-to-ambient equilibrium
    #
    # TODO: determine if the pack_to_ambient time constant should decrease with
    # the vehicle's velocity
    #
    # We hoist the following computations from our scalar inner loop.
    EMA_param <- ifelse(
      ((logtibble$charge_mode == 0) |
         (logtibble$est_pwr_a_c_50w == 0)),
      logtibble$EMA_parameter_module_to_ambient, # \lambda_p passive
      logtibble$EMA_parameter_module_AC_to_ambient # \lambda_a active
    )
    EMA_param_complement <- 1.0 - EMA_param

    # the following is a manual tail-call optimisation of the recursive filter
    # $x_t = x_{t-1}(1-\lambda) + h_t + a_t \lambda$ where $x_t$ is the pack
    # temperature, $h_t$ is the heatflow into the cells (in K), $a_t$ is the
    # ambient temperature, and $\lambda$ is the time constant for heatflow from
    # module to ambient (in units of the sampling_interval, rather than seconds
    # or hours)
    pred_temp_v <- logtibble$pack_avg_temp
    heat_in_v <- logtibble$pred_heating
    ambient_v <- logtibble$ambient
    for (i in seq(nsegments)[which(!wexclude)]) {
      prevpred <- pred_temp_v[wstart[i]]
      for (j in seq(wstart[i] + 1, wend[i])) {  # a scalar inner loop, ouch!
          nextpred <- prevpred * EMA_param_complement[j] +
          heat_in_v[j] +
          ambient_v[j] * EMA_param[j]
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
             waste_heatJ_kWh = cumsum(
               ifelse(is.na(pred_Joule_heating),
                      0.0,
                      pred_Joule_heating / 1000 / 3600)), # in kWh
             pred_polarisation_heating_kWh = cumsum(
               ifelse(is.na(pred_polarisation_heating),
                      0.0,
                      pred_polarisation_heating / 1000 / 3600)), # in kWh
             AC_energy_kWh = cumsum(
               ifelse(is.na(heat_pump_cooling),
                      0.0,
                      heat_pump_cooling / 1000 / 3600)), # in kWh
             .before = "cp1")
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
          ": MSE = ",
          round(MSE_of_fit(m), 4),
          " error range = (",
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

  if (trace > 0) {
    cat(" MSE =", round(MSE_of_fit(m), 2), "\n")
  }

  return(m)

}
