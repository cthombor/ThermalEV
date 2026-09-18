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
#' @param tmodel a thmodel, or a csv from LeafSpy (default: log26Jan2026.csv)
#' @param effective_pack_resistance in mOhms at 298.15K for SOC <= 70 percent
#' @param packr85 in mOhms, effective pack resistance at SOC = 85 percent
#' @param polarisation_rev in kJ/V, reversible (entropic) heat
#' @param polarisation_irr dimensionless, irreversible heat of polarisation
#' @param lambda_module_to_ambient in hours
#' @param lambda_module_AC_to_ambient in hours
#' @param fan_power in Watts
#' @param COP dimensionless
#' @param arrhenius_resistance in K, temperature dependence of packr
#' @param heat_capacity in kJ/K
#' @param gids_reserve dimensionless, for estimation of SOC from GIDs
#' @param ocv_tbl maps SOC onto OCV, either a 2-column tibble or an om_model
#' @param use_est_SOC use a GIDs-derived SOC to index ocv_tbl
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
#' m <- predict_temp()
#' m <- predict_temp(m, effective_pack_resistance = 67)

predict_temp <- function(tmodel = NULL,
                         effective_pack_resistance = NA,
                         packr85 = NA,
                         polarisation_rev = NA,
                         polarisation_irr = NA,
                         lambda_module_to_ambient = NA,
                         lambda_module_AC_to_ambient = NA,
                         fan_power = NA,
                         COP = NA,
                         arrhenius_resistance = NA,
                         heat_capacity = NA,
                         gids_reserve = NA,
                         ocv_tbl = NULL,
                         use_est_SOC = T,
                         iter_count = 4,
                         min_segment_length = 20,
                         trace = 2,
                         logfilnm = "log26Jan2026.csv",
                         logfildir = "data-raw",
                         from_date = NULL,
                         to_date = NULL,
                         from_idx = NULL,
                         to_idx = NULL) {

  # Notes on heat capacity:
  #
  # The Joule heating (in K) of a pack is the square of its amperage, multiplied
  # by its effective resistance and divided by its heat capacity (in J/K).
  # An accurate estimation of the effective resistance for a pack, based on its
  # observed thermal behaviour, is possible only with an accurate estimation
  # of its heat capacity.
  #
  # I recommend the thermal modelling of predict_temp(), with some judicious use
  # of fit_model(), be used to refine an initial estimate of heat capacity and
  # all other non-resistive parameters in my model, based on a fixed estimate of
  # the pack's effective resistance. With these refined estimates, the pack
  # voltage modelling of predict_volts() method can be used to refine the
  # resistance model.  Sourcing om_eNV24kWh.R and om_eNV50kWh.R will produce two
  # ocv_models (one for each size of pack), as required for the use of est_ocv()
  # and the optimisation routine fit_r_to_ocv().  These methods return a refined
  # ocv_model, notably including a revised ocv_table that maps the pack's SOC (as
  # reported by LeafSpy) onto the pack's (estimated) open circuit voltage (OCV).
  # The fit_r_to_ocv() method searches for better-fitting resistances to the
  # observed changes in pack voltage as a function of pack amperage, pack
  # temperature, SOC, and Hx. The fit_r_to_ocv() method also adjusts the
  # heat_capacity parameter so that the total Joule heating over the dataset
  # remains constant -- because (except for the reversible entropic heating and
  # the rather slow cooling processes) the change in pack temperature is
  # proportional to the rate of Joule heating divided by its heat_capacity.

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
    if ("ocv_model" %in% class(ocv_tbl)) {
      om <- ocv_tbl
      # om's parameters will be "pasted into" the thmodel if their values were
      # not specified in the call to predict_temp()
      if (is.na(effective_pack_resistance)) {
        effective_pack_resistance <-
          om$parameters[["effective_pack_resistance"]]
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
  if (!is.na(polarisation_rev)) {
    m$parameters[["polarisation_rev"]] <- polarisation_rev
  }
  if (!is.na(polarisation_irr)) {
    m$parameters[["polarisation_irr"]] <- polarisation_irr
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
  if (!is.na(gids_reserve)) {
    m$parameters[["gids_reserve"]] <- gids_reserve
  }

  # read a full set of params
  effective_pack_resistance <- m$parameters[["effective_pack_resistance"]]
  packr85 <- m$parameters[["packr85"]]
  polarisation_rev <- m$parameters[["polarisation_rev"]]
  polarisation_irr <- m$parameters[["polarisation_irr"]]
  lambda_module_to_ambient <- m$parameters[["lambda_module_to_ambient"]]
  lambda_module_AC_to_ambient <- m$parameters[["lambda_module_AC_to_ambient"]]
  fan_power <- m$parameters[["fan_power"]]
  COP <- m$parameters[["COP"]]
  arrhenius_resistance <- m$parameters[["arrhenius_resistance"]]
  heat_capacity <- m$parameters[["heat_capacity"]]
  gids_reserve <- m$parameters[["gids_reserve"]]
  ocv_tbl <- m$parameters[["ocv_tbl"]]

  if (trace > 0) {
    cat(paste0("predict_temp:",
               " c = ", round(heat_capacity, 5),
               ", prev = ", round(polarisation_rev, 5),
               ", pirr = ", round(polarisation_irr, 5),
               ", λp = ", round(lambda_module_to_ambient, 5),
               ", λa = ", round(lambda_module_AC_to_ambient, 5),
               ", fanp = ", round(fan_power, 5),
               ", COP = ", round(COP, 5),
               ", a = ", round(arrhenius_resistance, 5),
               ", r = ", round(effective_pack_resistance, 5),
               ", r85 = ", round(packr85, 5),
               ", gr = ", gids_reserve,
               "; "))
  }
  if (trace > 1) {
    cat("ocv_tbl:\n")
    print(ocv_tbl, max_footer_lines = 0)
  }

  if (COP < 0.0) {
    COP <- 0.0
    warning("The minimum COP of a fit is 0.0\n")
    # avoids a possible runaway negative COP in optim() if fan_power is low.
  }

  #compute delta_t for runs of near-consecutive samples
  logtibble <- logtibble |>
    mutate(delta_t = date_time - dplyr::lag(date_time))
  # n.b. dplyr's annoying redefinition of lag/lead is arguably intuitive, if
  # you imagine lag() as an element-wise operation which retrieves the
  # "previous" value in a vector, rather than taking a vector-centric view --
  # in which a vector is shifted "backwards" (toward lower-indexed/earlier
  # values) by a lag.  This is a direct -- and hazardous -- semantic conflict
  # with stats::lag() and xts::lag().  dplyr also masks first() and last(),
  # thereby creating additional hazards, unless you load the conflicted
  # package before loading dplyr.

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
        (abs((
          pack_avg_temp - dplyr::lead(pack_avg_temp)
        )) > 1),
      delta_t = ifelse(wonky_temps, NA, delta_t)
    )
  wwonky <- which(logtibble$wonky_temps)
  if (length(wwonky) > 0) {
    warning(paste(
      "Implausible temperature reading(s) at",
      paste(
        lubridate::format_ISO8601(logtibble$date_time[wwonky]),
        collapse = ", "
      ),
      collapse = " "
    ))
  }

  # rate of heat gain (in K/s)
  logtibble <- logtibble |>
    mutate(
      delta_K_delta_t =
        (pack_avg_temp - dplyr::lag(pack_avg_temp)) / delta_t,
      .before = cp1
    )

  # The slope $m$ of pack_amps, when integrated across the unit interval, adds
  # $m^2 / 2$ to the estimated Joule heating.  We estimate this slope using a
  # 2-point backward divided difference.
  logtibble <- logtibble |>
    mutate(
      slope_amps = (pack_amps - dplyr::lag(pack_amps)) / 2,
      slope_amps = ifelse(is.na(slope_amps) | is.na(delta_t), 0, slope_amps),
      .before = cp1
    )

  w <- which(is.na(logtibble$delta_t)[-length(logtibble$delta_t)])
  nsegments <- length(w)
  # delay the start of each segment, to avoid incomplete samples
  wstart <- w + 1
  wend <- dplyr::lead(w, default = length(logtibble$delta_t) + 1) - 1
  # avoid starting with a wonky temperature
  ww <- intersect(wstart, wwonky)
  if (length(ww) > 0) {
    warning(paste(
      "Delaying segment-start(s) due to wonky temperature reading(s) at",
      paste(lubridate::format_ISO8601(logtibble$date_time[ww]), collapse = ", "),
      collapse = " "))
    wstart = if_else(is.element(wstart, ww),
                     if_else(wstart < length(logtibble$delta_t),
                             wstart + 1, wstart),
                     wstart)
  }

  www <- intersect(wstart, wwonky)
  if (length(www) > 0) {
    warning(paste(
      "Ignoring entire segment(s) due to unstable temperature readings at",
      paste(lubridate::format_ISO8601(logtibble$date_time[www]), collapse = ", "),
      collapse = " "))
  }
  segexclude <- ((wend - wstart) < min_segment_length) |
    (is.element(wstart, www))

  if (sum(!segexclude) == 0) {
    warning("Insufficient segment lengths, no predictions will be made!")
  }

  segnumv = rep(0, length(logtibble$delta_t))
  for (i in seq(nsegments)[which(!segexclude)]) {
    segnumv[wstart[i]:wend[i]] = i
  }
  # n.b. segnum 0 is discontinuous, and we don't predict in it

  f_soc_to_ocv <- approxfun(m$parameters[["ocv_tbl"]],
                            method = "linear",
                            rule = 2)

  logtibble <- logtibble |>
    mutate(segnum = segnumv, .before = pack_avg_temp) |>
    group_by(segnum) |>
    mutate(
      sampling_interval = mean(delta_t, na.rm = T),
      # the sampling interval is a parameter in LeafSpy which we estimate on a
      # per-segment basis.

      pred_pack_avg_temp =
        if_else(segnum == 0, NA, dplyr::first(pack_avg_temp)),
      pred_hx =
        if_else(segnum == 0, NA, dplyr::first(hx)),
      # n.b. these time constants are in hours
      EMA_parameter_module_to_ambient =
        min(1.0, sampling_interval / (lambda_module_to_ambient * 3600)),
      EMA_parameter_module_AC_to_ambient =
        min(1.0, sampling_interval / (lambda_module_AC_to_ambient * 3600)),
      .before = pack_avg_temp
    ) |>
    ungroup()

  kWh_reserve <- gids_reserve * 0.080
  SOC_reserve <- kWh_reserve / m$capacity
  SOC_usable <- 1 - SOC_reserve
  logtibble <- logtibble |>
    mutate(
      est_ssoc = SOC_reserve +
        SOC_usable * gids * 0.080 / (m$capacity * soh / 100),
      # n.b. the reserve is of constant size in kWh (and in GIDs). The ratio of
      # SOC to GIDs scales with soh, within the usable range of SOC.  In OEM
      # firmware, the usable range of LeafSpy-reported SOC is 10% to 100%.
      #
      # Alistair's (early-rev) 50kWh BMS from VIVNE reports a SOC with a central
      # tendency that would put its 10% point at approximately 2.4 kWh.  This is
      # significantly less than a 10% reserve (= 30 GIDs on a 24kWh pack; 62.5
      # GIDs on a 50kWh pack).
      #
      # Both of the VIVNE-supplied BMS apparently compute SOC from something
      # other than (or in addition to) the GID, showing significant variances
      # from the formula above (esp. at SOC < 0.50 on my BMS).
      #
      # The dashboard displays a SOC with a different scaling.  Possibly: the dreaded turtle
      # displays on the dash when the LeafSpy-reported SOC would be at 15%,
      # i.e. 48 GIDs on a 24kWh pack.
      #
      # It's a confusing situation, especially with the unexplained variance
      # in SOC as reported from the VIVNE-supplied BMS firmware.
      #
      # See https://cthombor.wpcomstaging.com/50kwh-upgrade-to-
      # my-e-nv200/50kwh-upgrade-to-my-24kwh-2014-nissan-e-nv200-part-3-
      # estimation-of-usable-kwh/ for some data and discussion of GIDs,
      # LeafSpy-reported SOC, and dashboard-displayed SOC.
      #
      # TODO: Form an accurate and unbiased estimate of the pack's OCV as a
      # function of temperature and GIDs. Estimate an SOC from the cell
      # manufacturer's indicative rate-charging and -discharging voltage/time
      # curves.  Compare this estimated SOC with the one estimated here from
      # GIDs.
      ssoc = if (use_est_SOC)
        est_ssoc
      else
        (soc / 1e6)
      ,
      # n.b. ssoc is scaled to (0.0, 1.0)
      est_ocv = f_soc_to_ocv(ssoc),
      .before = pack_avg_temp
      # n.b. we estimate OCV from the kWh-based ssoc, rather than from an
      # estimate of Ah remaining.  There may be hidden parameters in the OEM
      # GID-estimator which allow it to be computed from the readout of a
      # coulomb-counter; alternatively, it may have no coulomb-counter but
      # instead it may be calculating a running-estimate of kWh consumption by
      # numerically integrating the products of readouts from a voltmeter and an
      # ammeter.

      # TODO: Estimate %Ah-remaining, and use it to key the lookup table of OCV.
    )
  if (trace == 2) {
    logvalid <- logtibble$segnum != 0
    GIDs <- logtibble$gids[logvalid]
    SSOC <- logtibble$est_ssoc[logvalid]
    `SOC/1e6` <- logtibble$soc[logvalid] / 1e6
    plot(GIDs,SSOC - `SOC/1e6`, main = tmodel$name)
    cat("SOH:\n")
    print(summary(logtibble$soh[logvalid]))
    cat("est_soc - LeafSpySOC:\n")
    print(summary(SSOC - `SOC/1e6`))
  }

  for (iternum in 1:iter_count) {
    # we perform an iterative approximation to the predicted temperatures, because
    # of the significant shifts in effective pack resistance as a function of
    # temperature.
    #
    # There are some shifts in the vehicle's estimated %Hx.  We work from
    # its value at the beginning of each prediction segment. The process which
    # updates estimates of %Hx is obscure, but could presumably be black-box
    # reverse-engineered with the aid of a simulation such as this one.
    #
    # We don't attempt to estimate pack_volts, but instead focus on modelling
    # the thermal behaviour of the pack from its timeseries of pack_amps and
    # pack_volts, the active cooling power, and the ambient temperature.  Our
    # e-NV200 traces do not include any from vehicles with a PTC pack heater, so
    # we do not attempt to model the thermal behaviour in sub-zero ambients.
    #
    # if iternum>1, we're using the previous prediction of temperature to
    # estimate the effective pack resistance (after the first sample).
    #
    # if iternum==1, we use the pack temperature at the beginning of a segment
    # to estimate the effective pack resistance for the whole of the segment.
    # The Arrhenius parameter (default: -3500) models a doubling of effective
    # resistance for every 18-degree drop in temperature.  A typical segment in
    # our simulations has less than a 10-degree shift in temperature, so the
    # convergence is typically nearly complete after a couple of iterative
    # updates to the temperature predictions of this simulation.
    for (i in seq(nsegments)[which(!segexclude)]) {
      logtibble$pred_pack_avg_temp[wstart[i]] <-
        logtibble$pack_avg_temp[wstart[i]]
    }

    sloper <- (packr85 - effective_pack_resistance) / 15
    # n.b. the pack is modelled as having a constant resistance for soc in (0%,
    # 70%); then linearly increasing with value packr85 at soc = 85%.  This
    # adjustment does not seem relevant as at v0.42; but we retain it "just in
    # case" further refinements of our model indicate a significant
    # SOC-dependency in the effective pack resistance.  Printouts of packr85 in
    # the titles of plots are suppressed when its value is equal to that of
    # effective_pack_resistance.
    logtibble <- logtibble |>
      group_by(segnum) |>
      mutate(
        eff_packr =
          ifelse(
            ssoc <= 0.70,
            effective_pack_resistance,
            effective_pack_resistance + sloper * (ssoc - 0.70)
          ) * exp(arrhenius_resistance *
                    (1 / 298.15 - 1 / (pred_pack_avg_temp + 273.15))) /
          (pred_hx / 100),
        pred_pack_volts = est_ocv - pack_amps * eff_packr / 1000,
        # we compute a (rough) estimate of the pack voltage as a function of
        # pack_amps, for use in fit_r_to_ocv().  n.b. this estimate is biased by
        # ionic-transport delays at high C rates, and also at low C rates
        # immediately after a high-C discharge (because cells require minutes to
        # equilibrate their ionic concentrations after polarisation shifts).
        # n.b. our resistance parameters are in mOhms, so we divide by 1000

        pred_Joule_heating =
          (pack_amps * pack_amps + 0.5 * slope_amps * slope_amps) *
          eff_packr / 1000 * delta_t, # in Ws
        # n.b. an accurate numerical integration of a quadratic function
        # requires an estimation of the slope of its dependent variable. In the
        # preamble to this loop, slope_amps was computed as a 2-point (backward)
        # divided difference.

        delta_v = pack_volts - dplyr::lag(pack_volts),
        delta_v = if_else(is.na(delta_v) | segnum == 0, 0.0, delta_v),
        pred_polarisation_heating_rev =
          delta_v * polarisation_rev * 1000, # in Ws
        # n.b. this is a reversible heat, causing the pack to heat somewhat
        # less when discharging at a given current than when charging at the
        # same rate.

        pred_polarisation_heating_irrev = pack_amps *
          (est_ocv - pack_volts) * polarisation_irr * delta_t, # in Ws
        # n.b. The irreversible heat of polarisation is always positive. When
        # charging, the overvoltage is positive and pack_amps is negative; when
        # discharging, the overvoltage is negative and pack_amps is positive.
        # Accordingly, we use the additive inverse of the estimated overvoltage
        # = (pack_volts - est_ocv) when estimating the heating in the formula
        # above. However the overvoltage while charging may be estimated as a
        # negative value, primarily due to inaccuracies in est_ocv, and also due
        # to a delayed response of pack_volts to a change in pack_amps.

        # TODO: consider revising est_ocv() so that it raises its estimate of
        # OCV(SOC), if necessary to avoid cases where the battery is observed
        # sourcing power at a voltage below its (currently-estimated) OCV,
        # except perhaps within a few minutes of a sign-shift in pack_amps.

        # TODO: consider estimating pack_voltage, rather than relying on
        # LeafSpy-traced voltages when estimating thermal behaviour.  But!
        # Additional parameters would be required -- at least two for Tafel's
        # equation, plus two more if a BV model is required to attain adequate
        # accuracy.  And that's just for the steady-state.  I doubt my dataset
        # is diverse enough to support such a complex modelling exercise, even
        # if I had the energy & motivation to do it.)

        # TODO: consider using Tafel's equation to estimate polarisation_irrev.
        # At present, est_ocv() uses an ohmic model, with the
        # effective_pack_resistance being its parameter.

        # TODO: consider adding yet-another time-constant to the model, so that
        # it is somewhat more accurate in its predictions of pack voltage when
        # pack_amps is highly variable. Equilibration of the ionic concentration
        # near cell electrodes, and of the temperature of the electrolyte, may
        # have time constants of a couple of minutes.

        cooling_power = 50 * est_pwr_a_c_50w - fan_power,
        cooling_power = ifelse(cooling_power < 0, 0, cooling_power),
        heat_pump_cooling = ifelse(
          charge_mode == 0,
          0, # AC is cooling the cabin
          COP * cooling_power * delta_t # AC is cooling the battery
        ), # in Ws

        # predict per-sample delta-heating of pack (in temperature K)
        # n.b. heat_capacity is in kJ/K == kWs/K
        # todo: consider adding a time-constant to delay the heating from
        # irreversible polarisation. We apply it immediately below, but it is
        # generated by ionic movement so has a time-constant of minutes.
        pred_heating = (pred_Joule_heating +
                          pred_polarisation_heating_rev +
                          pred_polarisation_heating_irrev -
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
    for (i in seq(nsegments)[which(!segexclude)]) {
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
    for (i in seq(nsegments)[which(segexclude)]) {
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
             pred_polarisation_heating_rev_kWh = cumsum(
               ifelse(is.na(pred_polarisation_heating_rev),
                      0.0,
                      pred_polarisation_heating_rev / 1000 / 3600)), # in kWh
             AC_energy_kWh = cumsum(
               ifelse(is.na(heat_pump_cooling),
                      0.0,
                      heat_pump_cooling / 1000 / 3600)), # in kWh
             .before = "cp1")
    m$logdata <- logtibble
    m$modified.last.time <- lubridate::now()

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
            lubridate::format_ISO8601(logtibble$date_time[minpew]),
            ",",
            lubridate::format_ISO8601(logtibble$date_time[maxpew]),
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
            lubridate::format_ISO8601(logtibble$date_time[minpe]),
            ",",
            lubridate::format_ISO8601(logtibble$date_time[maxpe]),
            ")\n"
        )
      }
    }
  }

  if (trace > 0) {
    cat(" MSE =", round(MSE_of_fit(m), 4), "\n")
  }

  return(m)

}
