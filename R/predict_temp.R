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
#' lSOC is the LeafSpy-reported soc, scaled by 1e6 to be in the range 0 to 1
#'
#' @param tmodel a thmodel, or a csv from LeafSpy (default: log26Jan2026.csv)
#' @param effective_pack_resistance in mOhms at 298.15K, == R1 + Rp
#' @param arrhenius_resistance, Arrhenius coefficient for R1 + Rp
#' @param entropic_heat in mV/K, reversible (entropic) heat
#' @param polarisation_Rp in mOhms at 298.15K
#' @param polarisation_R2 in mOhms at 298.15K
#' @param arrhenius_R2, Arrhenius coefficient for R2
#' @param lambda1_polarisation in s, decay rate of non-Faradaic polarisation
#' @param lambda2_polarisation in s, decay rate of Faradaic polarisation
#' @param lambda_module_to_ambient in hours
#' @param lambda_module_AC_to_ambient in hours
#' @param fan_power in Watts
#' @param COP dimensionless
#' @param heat_capacity in kJ/K
#' @param gids_reserve dimensionless, gids available at lSOC == 0
#' @param ocv_tbl maps lSOC or est_soc onto OCV, a 2-column tibble
#' @param ocv_relax may be increased, at risk of divergence
#' @param iter_count may be increased for a more accurate prediction
#' @param min_segment_length shorter sequences of samples are ignored
#' @param trace 0 for silent, 1 for minimal, 2 for verbose, 3 with a scatterplot
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
                         arrhenius_resistance = NA,
                         entropic_heat = NA,
                         polarisation_Rp = NA,
                         polarisation_R2 = NA,
                         lambda1_polarisation = NA,
                         lambda2_polarisation = NA,
                         lambda_module_to_ambient = NA,
                         lambda_module_AC_to_ambient = NA,
                         fan_power = NA,
                         COP = NA,
                         arrhenius_R2 = NA,
                         heat_capacity = NA,
                         gids_reserve = NA,
                         ocv_tbl = NULL,
                         ocv_relax = 1, # affects ep3 for OCV estimation
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
  # I recommend some manual optimisation of model parameters using
  # predict_temp(m), to refine the defaults set by m <- default_parameters(m),
  # before relying on the results of a "fine-tuning" optimisation of some
  # non-resistive model parameters by fit_model(m).
  #
  # fit_r_to_ocv(m) is another non-linear optimiser.  It returns a thmodel with
  # a revised estimate of the effective pack resistance, and with a revised heat
  # capacity (under the assumption that Joule heating is the primary
  # contribution to pack heating, in which case its heating is proportional to
  # its resistance and inversely proportional to its heat capacity). The
  # fit_r_to_ocv(m) optimisation is critically dependent on the accuracy of the
  # ocv_tbl parameter of the thmodel.  The effective pack resistance is the
  # "headline" parameter of the thmodel, but for the reasons sketched above it
  # covaries strongly with other modelling parameters... so I suggest you adjust
  # it only after optimising thermal and voltage fits using the other parameters
  # of a thmodel.  A re-optimisation of other thmodel parameters will be
  # important after even a modest change to the effective pack resistance.  Any
  # lack of convergence is suggestive of a defect either in the modelling (which
  # ignores many known factors) or its implementation.
  #
  # Another method of importance to parameter-tuning is revise_ocv(m), which
  # relies on the voltage predictions of m to estimate a revised ocv_tbl (with
  # monotonically-increasing values of OCV with respect to lSOC).  I recommend
  # caution when using this routine, because of the danger of overfitting
  # ocv_tbl to non-representative extremal sequences of LeafSpy readouts.  Note:
  # the LeafSpy-reported lSOC has a zero when the cells still have some reserve
  # capacity; and the dSOC (reported on the dashboard to the driver) has
  # yet-another scaling.   The "reserve" remaining at lSOC == 0 is yet-another
  # parameter in the model, by default set to 10% of the nominal kWh capacity of
  # the pack when its SOH == 1.
  #
  # TODO: use our (crude) voltage-modelling to estimate est_SOC independently
  # of the LeafSpy-reported soc.  I have misgivings about the accuracy of
  # the soc estimations of the VIVNE-supplied firmware, when lSOC is below 0.5.
  # A fully independent estimate of lSOC may shed some light on any bias or
  # any larger-than-usual unexplained variance in the firmware-reported soc.

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
    stopifnot(dim(ocv_tbl)[2] == 2) # sanity checks
    stopifnot(min(ocv_tbl[1]) >= 0.0)
    stopifnot(max(ocv_tbl[1]) <= 1.0)
    m$parameters$ocv_tbl <- ocv_tbl # update the thmodel's params
  }
  if (!is.na(effective_pack_resistance))
    m$parameters$effective_pack_resistance <- effective_pack_resistance
  if (!is.na(arrhenius_resistance))
    m$parameters$arrhenius_resistance <- arrhenius_resistance
  if (!is.na(entropic_heat))
    m$parameters$entropic_heat <- entropic_heat
  if (!is.na(polarisation_Rp))
    m$parameters$polarisation_Rp <- polarisation_Rp
  if (!is.na(polarisation_R2))
    m$parameters$polarisation_R2 <- polarisation_R2
  if (!is.na(arrhenius_R2))
    m$parameters$arrhenius_R2 <- arrhenius_R2
  if (!is.na(lambda_module_to_ambient))
    m$parameters$lambda_module_to_ambient <- lambda_module_to_ambient
  if (!is.na(lambda_module_AC_to_ambient))
    m$parameters$lambda_module_AC_to_ambient <- lambda_module_AC_to_ambient
  if (!is.na(lambda1_polarisation))
    m$parameters$lambda1_polarisation <- lambda1_polarisation
  if (!is.na(lambda2_polarisation))
    m$parameters$lambda2_polarisation <- lambda2_polarisation
  if (!is.na(fan_power))
    m$parameters$fan_power <- fan_power
  if (!is.na(COP))
    m$parameters$COP <- COP
  if (!is.na(heat_capacity))
    m$parameters$heat_capacity <- heat_capacity
  if (!is.na(gids_reserve))
    m$parameters$gids_reserve <- gids_reserve

  # read a full set of params
  effective_pack_resistance <- m$parameters$effective_pack_resistance
  arrhenius_resistance <- m$parameters$arrhenius_resistance
  entropic_heat <- m$parameters$entropic_heat
  polarisation_Rp <- m$parameters$polarisation_Rp
  polarisation_R2 <- m$parameters$polarisation_R2
  arrhenius_R2 <- m$parameters$arrhenius_R2
  lambda_module_to_ambient <- m$parameters$lambda_module_to_ambient
  lambda_module_AC_to_ambient <- m$parameters$lambda_module_AC_to_ambient
  lambda1_polarisation <- m$parameters$lambda1_polarisation
  lambda2_polarisation <- m$parameters$lambda2_polarisation
  fan_power <- m$parameters$fan_power
  COP <- m$parameters$COP
  heat_capacity <- m$parameters$heat_capacity
  gids_reserve <- m$parameters$gids_reserve
  ocv_tbl <- m$parameters$ocv_tbl

  if (trace > 0) {
    cat(paste0("predict_temp:",
               " c = ", round(heat_capacity, 5),
               ", eh = ", round(entropic_heat, 5),
               ", R = ", round(effective_pack_resistance, 5),
               ", Rp = ", round(polarisation_Rp, 5),
               ", R2 = ", round(polarisation_R2, 5),
               ", λp = ", round(lambda_module_to_ambient, 5), # passive cooling
               ", λa = ", round(lambda_module_AC_to_ambient, 5), # active A/C
               ",\n λt1 = ", round(lambda1_polarisation, 5),# RpC1 time-constant
               ", λt2 = ", round(lambda2_polarisation, 5), # R2C2 time-constant
               ", fanp = ", round(fan_power, 5),
               ", COP = ", round(COP, 5),
               ", aR = ", round(arrhenius_resistance, 5),
               ", aR2 = ", round(arrhenius_R2, 5),
               ", gr = ", gids_reserve,
               "\n"))
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

  if (polarisation_Rp > effective_pack_resistance - 1) {
    polarisation_Rp <- effective_pack_resistance - 1
    warning("Setting R1 to 1 mΩ, by reducing an excessively-large Rp",
            "to Reff - 1\n")
  }
  polarisation_C1 <- lambda1_polarisation *
    (effective_pack_resistance - polarisation_Rp) * polarisation_Rp /
    (effective_pack_resistance * 1000)
  polarisation_C2 <- lambda2_polarisation / (polarisation_R2 / 1000)
  # in F
  # n.b. we assume the capacitors in the EC are unaffected by temperature

  #compute delta_t for runs of near-consecutive samples
  logtibble <- logtibble |>
    arrange(date_time) |>
    mutate(delta_t = date_time - dplyr::lag(date_time))
  # n.b. dplyr's annoying redefinition of lag/lead is arguably intuitive, if you
  # imagine lag() as an element-wise operation which retrieves the "previous"
  # value in a vector, rather than taking a vector-centric view -- in which a
  # vector is shifted "backwards" (toward lower-indexed/earlier values) by a
  # lag, as in stats::lag() and xts::lag().  dplyr also masks first() and
  # last(), thereby creating additional hazards, unless you load the conflicted
  # package before loading dplyr.  Yet-another hazard is that dplyr::lag() and
  # dplyr::lead() have surprising semantics with grouped tibbles, see
  # https://stackoverflow.com/questions/
  # 28235074/dplyr-lead-and-lag-wrong-when-used-with-group-by
  # Well... one person's intuition of "natural semantics" in one context does
  # not always align with another person's intuition in another context!

  # we make a rude estimate of the sampling interval over the whole thmodel
  # in order to count missing samples (with reasonable accuracy)
  sampling_interval_est <- as.double(median(logtibble$delta_t, na.rm = TRUE))
  # multiple missing samples will terminate a predictive segment
  # n.b. time-stamps in the logs have a precision of 1 second

  # TODO: review this code for adequacy on thmodels with a non-constant sampling
  # interval (which may be changed at any time by the LeafSpy user). In
  # particular: the sampling interval was 5s in some of my recorded sessions,
  # and the predictions on these sessions may be distorted by unreliable records
  # which aren't detected by other heuristics.
  max_delta_t <- 2 * sampling_interval_est + 2
  logtibble <- logtibble |>
    mutate(delta_t = ifelse(delta_t > max_delta_t, NA, delta_t))

  # strangely, pack_t3_c is uniformly NA in all my logfiles.
  logtibble <- logtibble |>
    mutate(pack_avg_temp = rowMeans(across(c(
      pack_t1_c, pack_t2_c, pack_t4_c
    ))), .before = cp1)

  # n.b. pack temps are unreliable when LeafSpy is still initialising, as it
  # sometimes stutters on the previous temp readouts.  We rely heavily on the
  # first temp readings in a predictive segment as the basis of our temperature
  # predictions, so delay starting the prediction to make it more likely we're
  # starting with a valid temperature reading. see e.g. eNV200ac24kWh_2025
  # 2025-08-31 09:15:06
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

  # The slope m of pack_amps, when integrated across the unit interval, adds
  # (m^2/2)R to the estimated Joule heating. I estimate the slope at the
  # midpoint of each sampling interval using a first-order forward divided
  # difference.  This is NA at the end of each segment, which I adjust to 0 for
  # convenience when applying this additive correction to a numeric integration.
  # n.b. delta_t is NA in cases where sampling has been interrupted (as
  # indicated by a long interval between timestamps).  Additional accuracy
  # *might* be obtained with a symmetric divided difference
  # (sym_slope_pack_amps) and an estimate of the second derivative of pack_amps.
  logtibble <- logtibble |>
    mutate(
      slope_amps = dplyr::lead(pack_amps) - pack_amps,
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
  # n.b. segnum 0 is generally discontinuous, and we don't predict in it

  f_soc_to_ocv <- approxfun(ocv_tbl,
                            method = "linear",
                            rule = 2)

  f_ocv_to_soc <- approxfun(x = ocv_tbl$OCV,
                            y = ocv_tbl$SOC,
                            method = "linear",
                            rule = 2)

  f_ocv_to_dvdq <- approxfun(x = ocv_tbl$OCV,
                             y = ocv_tbl$DVDQ,
                             method = "linear",
                             rule = 2)

  logtibble <- logtibble |>
    mutate(segnum = segnumv, .before = pack_avg_temp) |>
    group_by(segnum) |>
    mutate(
      sampling_interval = if_else(segnum == 0, NA, mean(delta_t, na.rm = T)),
      # the sampling interval is a parameter in LeafSpy which I estimate on a
      # per-segment basis.  delta_t has a 1-second granularity, so I use its
      # mean rather than its median to gain some additional precision in its
      # central tendency.
      # TODO: if the mean differs greatly from the median, this suggests very
      # irregular sampling -- which could significantly increase error in the
      # simulation if the timing irregularities are correlated with the
      # magnitude of shifts in pack voltage or current.  This seems very
      # unlikely, but perhaps a cursory investigation would still be merited?
      # For example: the EMA parameter might be adjusted on a per-sample basis,
      # and this should reduce any bias from correlations of sampling frequency
      # with the quantity being time-averaged.

      pred_pack_avg_temp =
        if_else(segnum == 0, NA, dplyr::first(pack_avg_temp)),
      pred_hx =
        if_else(segnum == 0, NA, dplyr::first(hx)),
      EMA_parameter_module_to_ambient =
        min(1.0, sampling_interval /
              (lambda_module_to_ambient * 3600)), # parameterised in hours
      # n.b. the scaling of EMA_parameters is per-segment
      EMA_parameter_module_AC_to_ambient =
        min(1.0, sampling_interval /
              (lambda_module_AC_to_ambient * 3600)),
      # EMA_parameter_p1 =
        # min(1.0, sampling_interval / lambda1_polarisation),
      # EMA_parameter_p2 =
        # min(1.0, sampling_interval / lambda2_polarisation),
      # n.b. the polarisation time constants are adjusted below for pack_temp
      # TODO: see if adequate accuracy is obtained without this fiddly
      # adjustment for pack_temp, in which case the code above should be
      # reverted.

      .before = pack_avg_temp
    ) |>
    ungroup()

  kWh_reserve <- gids_reserve * 0.080
  SOC_reserve <- kWh_reserve / m$capacity
  SOC_usable <- 1 - SOC_reserve

  logtibble <- logtibble |>
    group_by(segnum) |>
    mutate(
      lSOC = soc / 1e6,
      # scale this 20-bit integer to a 0-1 range

      est_lSOC = SOC_reserve +
        SOC_usable * gids * 0.080 / (m$capacity * soh / 100),
      # est_lSOC is an attempt to mimic the computation soc by the OEM BMS for
      # the 24kWh Leaf and e-NV200.  The gids_reserve (the intercept of the
      # straightline portion of the curve) is apparently of constant size, so
      # that the kWh in "reserve" is unaffected by SOH.  Some corrections to the
      # slope occur at values of lSOC below 50%.
      #
      # To add to the confusion, the initial slope of the SOC displayed on the
      # dashboard, as well as its intercept "initial reserve", is different to
      # the slope & intercept on the LeafSpy-reported SOC with an OEM BMS.  With
      # my VIVNE-supplied BMS firmware, there's a quirky behaviour with massive
      # "corrections" below 50% SOC.  Alistair's early-rev 50 kWh firmware
      # apparently has a different method for computing SOC from GIDs.
      #
      # Alistair's (early-rev) 50kWh BMS from VIVNE reports a SOC with a central
      # tendency that would put its reserve at 2.4 kWh.  This is significantly
      # less than the 10% reserve (= 30 GIDs on a 24kWh pack) of the intercept
      # of the straight-line estimation of SOC from GIDs on my 24kWh Leaf and
      # e-NV200.  The straight-line estimation of high values of SOC from gids
      # allows for a 10% reserve (62.5 GIDs) on my 50kWh pack.
      #
      # See https://cthombor.wpcomstaging.com/50kwh-upgrade-to-
      # my-e-nv200/50kwh-upgrade-to-my-24kwh-2014-nissan-e-nv200-part-3-
      # estimation-of-usable-kwh/ for some data and discussion of GIDs,
      # LeafSpy-reported SOC, and dashboard-displayed SOC.

      # TODO: Study the inaccuracies of soc-estimation by my VIVNE-supplied BMS.

      est_ocv_from_lSOC = f_soc_to_ocv(lSOC),
      # est_ocv_delta = - pack_amps * sampling_interval *
      #   f_ocv_to_dvdq(est_ocv_from_lSOC),
      # n.b. this is very inaccurate when driving (due to high-freq spikes in
      # pack_amps and pack_volts which are unrecorded in the 15-sec samplings)
      est_ocv_delta = est_ocv_from_lSOC -
        lag(est_ocv_from_lSOC, default = first(est_ocv_from_lSOC)),
      # lSOC has high resolution, but drifts significantly between
      # recalibrations. This delta spikes every time lSOC is recalibrated, so
      # must be used cautiously.
      est_ocv_from_pack_behaviour = pack_volts +
        pack_amps * effective_pack_resistance / 1000,
      # an initial estimate, will be refined through simulation.  Assumes no
      # polarisation currents and a pack temperature of 25 degrees. A more
      # accurate initial estimate could be made by temperature-correcting Reff
      # at the start of each predictive segment.

      # est_ocv_from_pack_behaviour = pack_volts + cumsum(est_ocv_delta),
      # an alternative estimate, but relies heavily on a numerical integration
      # of f_ocv_to_dvdq() and is heavily biased by the starting pack_volts in
      # each segment.

      est_Joule_heating_amperage = pack_amps,
      # n.b. This sampled data includes ripples from the polarisation currents
      # which haven't yet been estimated.

      # for an equivalent circuit (R1-(Rp//C1))//(R2-C2)
      est_polarisation_amperage_p = 0,
      # current attributed to a discharge or charge of C1
      est_polarisation_amperage_2 = 0,
      # current attributed to a discharge or charge of C2
      est_voltage_C1 =
        -pack_amps * polarisation_Rp / effective_pack_resistance,
      # voltage drop across C1, assuming no polarisation current

      # n.b. currents which charge the pack are negative, by Nissan convention
      # (as reported by LeafSpy), causing Ohm's Law to be E = -IR in our
      # simulation. Kirchhoff's Laws are unaffected by this sign change.

      est_voltage_C2 = est_ocv_from_pack_behaviour,
      # no charge on C2, so a voltage drop of pack_volts - OCV across R2
      .before = pack_avg_temp
    ) |>
    ungroup()

  for (iternum in 1:iter_count) {
    # This loop computes an iterative approximation to the predicted
    # temperatures.
    #
    # There are some shifts in the vehicle's estimated %Hx, but I work from
    # its value at the beginning of each prediction segment. The process which
    # updates estimates of %Hx is obscure, but could presumably be black-box
    # reverse-engineered with the aid of a simulation such as this one.
    #
    # if iternum==1, I use the pack temperature at the beginning of a segment
    # to estimate the effective pack resistance for the whole of the segment.
    # I also rely on the LeafSpy-reported lSOC and the ocv_tbl for an initial
    # estimate of the ocv at the time of each sample point.
    #
    # if iternum>1, I use the previous prediction of temperature to
    # estimate the effective pack resistance (after the first sample).  I
    # also revise the estimate of ocv, in an attempt to find a more accurate
    # estimate of SOC as well as an indication of errors in ocv_tbl.
    #
    # n.b. convergence is *not* guaranteed!  Parameters of the equivalent
    # circuit must be well-estimated.  It may be helpful to start with a
    # very large value for polarisation_R2 and a very small value for
    # polarisation_Rp, so that polarisation effects are neglected.  Also
    # note: the collinearity of the lambda_polarisation parameters with
    # the corresponding time constants will require both to be shifted when
    # searching for best fits, if the (implicit) values for C1 and C2 are
    # already accurate.
    #
    # The Arrhenius parameters model a doubling of effective resistance for
    # every 18-degree drop in temperature in their default value of -3500.  Few
    # if any segments in our simulations have more than an 18-degree shift in
    # temperature. The convergence is thus (typically) nearly complete after a
    # couple of iterative updates to the temperature predictions, so we set
    # iter_count to 4 by default.  Furthermore we use the mean value of pack
    # temperature over each segment when temperature-adjusting the lambda1 and
    # lambda2 time constants for polarisation -- this is a programming
    # convenience, as the C-coded implementation of an EMA in stats::filter()
    # requires lambda to be scalar.

    logtibble$pred_pack_avg_temp[wstart[!segexclude]] <-
      logtibble$pack_avg_temp[wstart[!segexclude]]
    logtibble <- logtibble |>
      group_by(segnum) |>
      mutate(
        delta_Q = (lSOC - lag(lSOC, default = first(lSOC))) *
          (a_hr / 10000) *
          3600, # delta of pack charge, in coulombs

        arr_factor =
          exp(arrhenius_resistance *
                (1 / 298.15 - 1 / (first(pred_pack_avg_temp) + 273.15))) /
          (first(pred_hx) / 100),
        # n.b. we use only the first pack_avg_temp and the first hx of each
        # segment in our first estimation of the temperature-corrected
        # equivalent resistances

        eff_packr_tc = arr_factor * effective_pack_resistance,

        rp_tc = arr_factor * polarisation_Rp,
        # TODO: validate the temperature-corrected Rp using the same Arrhenius
        # constant for Rp as for the series combination R1 + Rp = Reff.  For
        # adequate accuracy, it may be necessary to have an explicit R1
        # parameter with its own temperature compensation, and to compute Reff
        # as the sum of R1 and Rp. However this may make it more difficult to
        # drop back to the simpler EC of R1-C1; at present this can be done
        # simply when calling predict_temp(), by setting Rp = Reff and lambda2
        # = 0.

        r1_tc = eff_packr_tc - rp_tc,

        l1_tc = arr_factor * lambda1_polarisation,
        ep1_tc = min(1.0, delta_t / l1_tc),
        # recomputed for each iteration, shifting as the pack temp evolves

        arr_factor2 =
          exp(arrhenius_R2 *
                (1 / 298.15 - 1 / (first(pred_pack_avg_temp) + 273.15))) /
          (first(pred_hx) / 100),
        r2_tc = arr_factor2 * polarisation_R2,         ,
        # TODO: is arrhenius_R2 important to the accuracy of our model?  Seems
        # fiddly... and having yet-another parameter is opening yet-another door
        # to overfitting, as well as wasting resource in a non-linear
        # optimisation and confusing the reportage of a best-fit with
        # adjustments of dubious validity to a parameter which affects
        # the timing of pack heating by a matter of seconds.

        l2_tc = arr_factor2 * lambda2_polarisation,
        ep2_tc = min(1.0, delta_t / l2_tc),
        # n.b. the effective resistance for lambda2_polarisation is r2

        delta_pack_amps = pack_amps -
          dplyr::lag(pack_amps, default = first(pack_amps)),

        mean_pack_amps = (
          pack_amps + dplyr::lag(pack_amps, default = first(pack_amps))
          ) / 2,
        lag_delta_t = dplyr::lag(delta_t, default = NA),
        lead_delta_t = dplyr::lead(delta_t, default = NA),
        sym_slope_pack_amps =
          (dplyr::lead(pack_amps, default = last(pack_amps)) -
           dplyr::lag(pack_amps, default = first(pack_amps))) /
          (lead_delta_t + lag_delta_t),
        # n.b. this estimate of the slope is unbiased by the second derivative
        # of the sampled function *if* the sampling interval is constant --
        # an assumption which is often violated esp. when the sampling interval
        # is 8 seconds (as in segments 803 and 804 of eNV50kWh.rda)

        impulse_a = (pack_volts - est_ocv_from_pack_behaviour) / eff_packr_tc -
          pack_amps,
        # n.b. pack_volts responds immediately to changes in load, with impulses
        # causing polarisation currents which are buffered in the RC network of
        # our equivalent circuit.  At steady-state, there are no polarisation
        # currents and pack_amps is the overvoltage divided by the effective
        # pack resistance.  We estimate the OCV from the pack's behaviour (as
        # modelled in our EC), rather than from lSOC (which has an obscure
        # scaling, as well as being subject to inaccuracies of roughly 10% which
        # are occasionally corrected).
        impulse_a = if_else(is.na(impulse_a), 0, impulse_a),
        # n.b. zero impulse current at the start of a segment
        impulse_q = impulse_a * delta_t,
        impulse_q = if_else(is.na(impulse_q), 0, impulse_q),

        .before = pack_avg_temp
      ) |>
      ungroup()

    for (i in seq(nsegments)[which(!segexclude)]) {
      ia <- logtibble$pack_amps[wstart[i]:wend[i]] # instantaneous amperage
      iv <- logtibble$pack_volts[wstart[i]:wend[i]] # instantaneous voltage

      si <- logtibble$sampling_interval[wstart[i]] # mean sampling interval
      dq <- logtibble$delta_Q[wstart[i]:wend[i]] # delta of pack-charge, in As
      dpa <- logtibble$delta_pack_amps[wstart[i]:wend[i]]

      impulse_a <- logtibble$impulse_a[wstart[i]:wend[i]]
      # current attributed to unsampled spikes or valleys in pack_amps
      impulse_q <- - logtibble$impulse_q[wstart[i]:wend[i]]

      reff <- logtibble$eff_packr_tc[wstart[i]:wend[i]] / 1000
      r1 <- logtibble$r1_tc[wstart[i]:wend[i]] / 1000
      r2 <- logtibble$r2_tc[wstart[i]:wend[i]] / 1000
      rp <- logtibble$rp_tc[wstart[i]:wend[i]] / 1000
      # temperature-adjusted resistances, in Ohms

      ep1 <- logtibble$ep1_tc[wstart[i]:wend[i]] # vector-valued
      ep2 <- logtibble$ep2_tc[wstart[i]:wend[i]] # vector-valued
      ep3 <- ocv_relax * min(ep1, ep2) # scalar, as required by stats::filter()
      # temperature-adjusted lambdas (already converted to EMA parameters)

      # n.b. we use ep3 in a low-pass stats::filter() on ocv_prime,
      # to smooth its variations due to polarisation currents

      # we copy into per-segment vectors, to aid debugging
      ocv <- logtibble$est_ocv_from_pack_behaviour[wstart[i]:wend[i]]
      ocv_delta <- ocv - dplyr::lag(ocv, default = ocv[1]) +
        logtibble$est_ocv_delta[wstart[i]:wend[i]]

      iac <- logtibble$est_Joule_heating_amperage[wstart[i]:wend[i]]
      # estimated inst. charging current, from prior iteration or initialisation

      iap <- logtibble$est_polarisation_amperage_p[wstart[i]:wend[i]]
      ia2 <- logtibble$est_polarisation_amperage_2[wstart[i]:wend[i]]
      # estimated polarisation currents, from prior iteration or initialisation

      iv1 <- logtibble$est_voltage_C1[wstart[i]:wend[i]]
      iv2 <- logtibble$est_voltage_C2[wstart[i]:wend[i]]
      # estimated voltage at C1 (resp. C2)

      # n.b. Our prefix "prime_" is intended to connote an update to an existing
      # vector (e.g. ocv) which is being constructed in this iteration of
      # our (hopefully-convergent!) simulation.
      ocv_prime <-
        as.vector(
          stats::filter(
            ep3 * (iv - iac * r1 - (iac - iap) * rp),
            # n.b. the current through R1 is iac, negated as per the
            # Nissan/LeafSpy convention. The current through Rp is iac - iap,
            # where iap is the charging current for C1. For consistency with the
            # "negative current" convention elsewhere in the model, C1 is
            # charged when iap is negative.  This estimate is independent
            # of lSOC if the simulation is convergent; but the rate of
            # convergence depends on lSOC having a stable bias that is
            # small enough that a lookup in ocv_tbl gives a good estimate
            # of dV/dSOC where V is the pack voltage and SOC is its state
            # of charge as estimated by the BMS (and as reported by LeafSpy).
            filter = c(1 - ep3),
            method = "recursive",
            # exponential smoothing, with time constant si/ep3
            init = ocv[1]
          ))
      # The low-pass EMA on ocv_prime is intended to filter out the voltage
      # perturbations arising from the polarisation currents.  This should be
      # an unbiased estimate, because the average of the polarisation currents
      # tends toward zero.

      overvoltage <- iv - ocv_prime

      # calculations for the R2-C2 branch of the EC
      iac_prime <- rep(NA, wend[i] - wstart[i] + 1)
      ia2_prime <- rep(NA, wend[i] - wstart[i] + 1)
      iv2_prime <- rep(NA, wend[i] - wstart[i] + 1)
      Q2 <- rep(NA, wend[i] - wstart[i] + 1) # coulomb-counter on C2
      impulse_q2 <- impulse_q * ep2 / (ep1 + ep2) # impulse_q into C2
      prev_ia2 <- impulse_q2[1] / si
      prev_iv2 <- ocv_prime[1]
      prev_Q2 <- prev_iv2 * polarisation_C2 + impulse_q2[1]
      # TODO: analyse impulse_q to find an adequate heuristic for distinguishing
      # a drift-correction from an unrecorded spike in pack current.
      for (j in seq(1, wend[i] - wstart[i] + 1)) {
        delta_iv2 <- (ocv_prime[j] + ocv_delta[j] - prev_iv2) * ep2[j]
        # n.b. ocv_delta is a high-frequency adjustment to our low-pass ocv
        # estimate.
        prev_Q2 <- Q2[j] <- prev_Q2 +
          delta_iv2 * polarisation_C2 +
          impulse_q2[j]
        prev_iv2 <- iv2_prime[j] <- prev_Q2 / polarisation_C2
        #        prev_iv2 <- iv2_prime[j] <- prev_iv2 + delta_iv2
        # iv2 relaxes toward the (currently estimated) OCV, disrupted by
        # the high-frequency impulse_q
        prev_ia2 <- ia2_prime[j] <- - delta_iv2 / r2[j] - impulse_q2[j] / si
        # n.b. negative (Nissan-convention) currents, inverting Ohm's Law
        }
      # TODO: consider replacing the scalar loop above with a call to the
      # C-coded stats::filter(), for decreased runtime at the cost of
      # accuracy in cases where the temperature-adjusted resistance r2 varies
      # significantly within a segment.
      # TODO: consider elaborating stats::filter() to support a matrix-valued
      # filter parameter.  Such an EMA would be sufficient to calculate Q1_prime
      # and Q2_prime efficiently (on a superscalar CPU) and with full accuracy,
      # using separately-compiled "helper" functions which memorialise the
      # values of iap_prime, iv1_prime, ia2_prime, and iv2_prime.
      # DONE: Not a worthwhile project.  R is a poor choice of computing
      # environments for the kernel of a signal processing calculation, so I
      # doubt there'll be much of a signal-processing market for this mod.
      # Furthermore, calculations on time series are poorly supported in the
      # tidyverse, and I think that'd be important "infrastructure" to build
      # before "investing" in upgrades to stats::filter() -- especially given
      # the semantic confusion from the (IMHO rather strange) redefinition of
      # "lag" and "lead" within the tidyverse... a confusion which R seems to
      # have inherited from S. Transforming the tidyverse into a
      # semantically-coherent environment for calculations on timeseries is far
      # far beyond my paygrade! So... I'll leave it to the statistical computing
      # community to upgrade stats::filter() to implement the "adaptive simple
      # exponential smoothing method" of equation 1 in J. Forecast. 23, 385–404
      # (2004).

      iac_prime <- ia - ia2_prime + impulse_a
      # n.b. by Kirchhoff's first law, ia == iac + ia2. impulse_a is unmeasured
      # at sampling times, but we're not subsampling in this simulation so
      # we just add it here.  ia2_prime is net of the portion of the
      # impulse_a which was absorbed by the R2-C2 branch of the EC.

      # calculations for the R1-(Rp//C1) branch of the EC
      iv1_prime <- rep(NA, wend[i] - wstart[i] + 1)
      iap_prime <- rep(NA, wend[i] - wstart[i] + 1)

      Q1 <- rep(NA, wend[i] - wstart[i] + 1) # coulomb-counter on C1
      # n.b. the above vectors are for debugging purposes only.  Our manual
      # unwinding of an iterative solution of the state equations has five
      # scalar loop-carried dependencies, as listed below.

      # TODO: consider rewriting the scalar loops in C, for an efficient
      # execution on the superscalar CPU used in contemporary laptop and
      # desktop computers.
      # DONE: The cost is 1 SMOP ("small matter of programming"), plus a
      # significant loss in maintainability and readibility. The benefit is a
      # more rapid execution of the simulation; but on my longest logfiles its
      # performance is (barely) acceptable for repeated executions by a the
      # nonlinear optimisation routine fit_model().  The cost thus greatly
      # exceeds the benefits for this experimental code.

      prev_iap <- 0 # we assume no polarisation currents at segment-starts
      prev_iv1 <- - iac_prime[1] * rp[1] / reff[1]
      # (inferred) voltage across C1, with R1-Rp as a voltage divider of the
      # "negative current" iac
      prev_Q1 <- prev_iv1 * polarisation_C1
      # n.b. C1 stores charge with the usual sign convention: Q1 = V1 * C1.

      # n.b. Our prefix "i" is intended to connote "instantaneous".  Our
      # prefix "prev_" is intended to connote a loop-carried dependency i.e. a
      # value which was computed in the previous iteration of the loop (or
      # in its initialisation), and which is updated in this iteration.

      for (j in seq(1, wend[i] - wstart[i] + 1)) {
        v1_zero <- - (ia[j] - ia2[j]) * rp[j] / reff[j]
        delta_iv1 <- (v1_zero - prev_iv1) * ep1[j]
        # iv1 relaxes toward v1_zero (reaching it if iap == 0)
        prev_iv1 <- iv1_prime[j] <- prev_iv1 + delta_iv1
        prev_iap <- iap_prime[j] <- - delta_iv1 * polarisation_C1 / si
        prev_Q1 <- Q1[j] <- prev_Q1 - prev_iap * si
        # n.b. the charge on Q1 is unknown at the start of a segment, but should
        # converge on an accurate value as the polarisation currents and the
        # estimaged overvoltage relax toward accurate values.  If prev_Q1 is
        # nonconvergent (i.e. oscillating wildly) on a dataset, perhaps due to a
        # sampling rate that's slower than the RC time constants in the EC, then
        # subsampling and/or a few steps of a Newton-Raphson method might be
        # sufficient to estimate convergent values for the polarisation
        # currents.
      }

      DEBUG = T
      if ((DEBUG && any(abs(iv1_prime) > 100)) ||
          (DEBUG && any(abs(iv2_prime - ocv_prime) > 100)) ||
          ((tmodel$name == "eNV50kWh") &&
           ((i == 10) || (i == 672) || (i == 913)))) {
        # n.b. excessive violations of Kirchhoff's First Law, or an excessive
        # voltage on C1 or C2, are indicative of a highly-inaccurate simulation,
        # perhaps worthy of an immediate investigation.
        # n.b. a few segments from eNV50kWh are snapshotted for inspection

        dt <- logtibble$delta_t[wstart[i]:wend[i]]
        ls <- logtibble$lSOC[wstart[i]:wend[i]]
        eod <- logtibble$est_ocv_delta[wstart[i]:wend[i]]
        pat <- logtibble$pack_avg_temp[wstart[i]:wend[i]]
        ppat <- logtibble$pred_pack_avg_temp[wstart[i]:wend[i]]
        diagt <- tibble(ia, iv,
                        ocv, ocv_prime, ocv_delta, overvoltage,
                        iv1, iv1_prime, iv2, iv2_prime,
                        iac, iac_prime,
                        iap, iap_prime, ia2, ia2_prime,
#                        impulse_a, impulse_q, impulse_q2,
                        Q1, Q2,
                        dt, ls, eod, pat, ppat,
                        reff, r1, rp, r2,
                        ep1, ep2, ep3)
        write.csv(diagt, paste0("seg", i, ".", iternum, ".csv"))
        if (any(abs(iap) > 1000) || any(abs(ia2) > 1000)) {
          cat("Excessive current flow.  Aborting.\n")
          browser()
        }
      }
      # TODO: replace this diagnostic code with a stopifnot(), prior to release

      logtibble$est_Joule_heating_amperage[wstart[i]:wend[i]] <- iac_prime
      # charging/discharging amperage of traction pack
      logtibble$est_polarisation_amperage_p[wstart[i]:wend[i]] <- iap_prime
      # polarisation amperages with time constant lambda1
      logtibble$est_polarisation_amperage_2[wstart[i]:wend[i]] <- ia2_prime
      # polarisation amperage through r2, with time constant lambda2
      logtibble$est_ocv_from_pack_behaviour[wstart[i]:wend[i]] <- ocv_prime
      # adjusted estimate of ocv
      logtibble$est_voltage_C1[wstart[i]:wend[i]] <- iv1_prime
      logtibble$est_voltage_C2[wstart[i]:wend[i]] <- iv2_prime
      # estimated voltages across C1 (resp. C2), for debugging
    }


    # TODO: consider transmogrifying the code above into the tidyverse. DONE:
    # Not a good idea.  Estimated cost: 1 SMOP.  Estimated ROI: negative. It's
    # far from clear to me whether the tidyverse will support grouped calls to
    # stats::filter() without ugly workarounds.  More importantly: the code
    # above aids diagnostic analyses from the browser, as well as being more
    # amenable to modification into a more accurate (but even less tidy)
    # simulation with polarisation lambdas which vary within an interval with
    # pack temperature. Such a simulation is neither vectorisable nor supported
    # by stats::filter(), but it could be tidily expressed in a language (e.g.
    # C) with an optimising compiler capable of performing loop unrolling for
    # efficient execution on a superscalar CPU.

    logtibble <- logtibble |>
      group_by(segnum) |>
      mutate(
        est_Joule_heating =
          delta_t *
          est_Joule_heating_amperage * est_Joule_heating_amperage *
          (eff_packr_tc / 1000),
        # in Ws

        est_Joule_heating_slope_corr =
          delta_t *
          (est_Joule_heating_amperage * est_Joule_heating_amperage +
             slope_amps * slope_amps / 2) * (eff_packr_tc / 1000),
        # TODO: is this correction helpful, irrelevantly small, or badly skewed
        # due to short-term variations in polarisation currents dominating the
        # deltas in pack_amps? Maybe estimate the first derivative of all
        # identified currents? Maybe the short time constant of
        # est_polarisation_amperage_2 response to a shift in pack voltage makes
        # its first derivative tricky to estimate in an unbiased way?
        # TODO: consider adding a variance correction, derived from delta_Q

        est_heating_rp =
          delta_t *
          (est_Joule_heating_amperage - est_polarisation_amperage_p) *
          (est_Joule_heating_amperage - est_polarisation_amperage_p) *
          rp_tc / 1000,

        est_heating_r2 =
          delta_t *
          est_polarisation_amperage_2 * est_polarisation_amperage_2 *
          r2_tc / 1000,
        # TODO: consider explicitly subsampling iv2 and ia2, to estimate
        # heating from current flows which fluctuate at a frequency higher
        # than the sampling rate.  Alternatively: estimate the RMS of ia2.
        # In est_heating_r2, we are squaring a mean current (averaged over
        # a sampling interval), thereby underestimating the mean of the squared
        # current.

        est_soc = f_ocv_to_soc(est_ocv_from_pack_behaviour),
        # This may shed light on the apparently-wonky estimation of soc
        # by the reflashed firmware in the 50kWh BMS.

        est_entropic_heating =
          delta_t *
          est_Joule_heating_amperage *
          (pack_avg_temp + 298.15) * (entropic_heat +
                                        graphite_entropy(est_soc) + 60) /
          1000, # in W-s
        # n.b. this is a reversible heat, causing the pack to heat somewhat
        # less (and possibly even to cool) when discharging than when charging
        # at the same rate.

        # n.b. the sign of pack_amps is reversed from the convention in
        # the technical literature on lithium-ion cells, causing a sign-change
        # in the entropic coefficient.

        # we use an experimentally-derived entropy in an attempt to reduce the
        # variance of estimation.  The entropy change of the graphite anode is
        # roughly -60 mV/K at SOC == 0.5, with an SOC-related variance of about
        # 20 mV/K.  We offset the lSOC-derived entropy by 60 so that the value
        # of the entropic_heat parameter is a mid-SOC level that is "adjusted"
        # for SOC.

        # n.b. the entropy change of an NMC cathode is dependent on its
        # NMC ratios, its doping, and the electrolyte; and may be quite
        # large for low SOC.

        # TODO: analyse low-SOC segments in the LeafSpy traces, to see if
        # their thermal evolution might be confidently attributed to an
        # extraordinarily-large entropic heating or cooling.

        est_entropic_heating_alt =
          delta_t *
          est_Joule_heating_amperage *
          (pack_avg_temp + 298.15) * (entropic_heat +
                                        graphite_entropy(lSOC) + 60) /
          1000, # in W-s, using lSOC rather than est_soc

        est_entropic_heating_alt_alt =
          delta_t *
          est_Joule_heating_amperage *
          (pack_avg_temp + 298.15) * entropic_heat / 1000,
        # in Ws, with no SOC "correction" to the entropic factor.

        cooling_power = 50 * est_pwr_a_c_50w - fan_power,
        cooling_power = ifelse(cooling_power < 0, 0, cooling_power),
        heat_pump_cooling = ifelse(
          charge_mode == 0,
          0, # AC is cooling the cabin
          delta_t * COP * cooling_power # AC is cooling the battery
        ), # in Ws

        # estimate per-sample delta-heating of pack (in temperature K)
        # n.b. heat_capacity is in kJ/K == kWs/K
        est_heating = (est_Joule_heating +
                          est_entropic_heating +
                          est_heating_rp +
                          est_heating_r2 -
                          heat_pump_cooling
                        ) / (heat_capacity * 1000),
        .before = cp1
      ) |>
      ungroup()

    # n.b. when the fan is running inside the pack, the module-to-pack thermal
    # conductivity is significantly increased -- so the time constant is
    # significantly shorter on the module-to-ambient equilibrium
    #
    # TODO: determine if the pack_to_ambient time constant should decrease with
    # the vehicle's velocity
    EMA_param <- ifelse(
      ((logtibble$charge_mode == 0) |
         (logtibble$est_pwr_a_c_50w == 0)),
      logtibble$EMA_parameter_module_to_ambient, # \lambda_p "passive"
      logtibble$EMA_parameter_module_AC_to_ambient # \lambda_a "active"
    )
    EMA_param_complement <- 1.0 - EMA_param

    pred_temp_v <- logtibble$pack_avg_temp
    heat_in_v <- logtibble$est_heating
    ambient_v <- logtibble$ambient
    impulse_heat <- heat_in_v + EMA_param * ambient_v

    # the following is a manual tail-call optimisation of the recursive filter
    # $x_t = x_{t-1}(1-\lambda) + h_t + a_t \lambda$ where $x_t$ is the pack
    # temperature, $h_t$ is the heatflow into the cells (in K), $a_t$ is the
    # ambient temperature, and $\lambda$ is the time constant for heatflow from
    # module to ambient (in units of the sampling interval, rather than seconds
    # or hours).
    for (i in seq(nsegments)[which(!segexclude)]) {

#    lambda <- EMA_param[[wstart[i]]] # per-segment!
     # if lambda were constant over a segment, stats::filter() would be
     # elegant and efficiently-executed on any superscalar CPU, e.g.
#    pred_temp[wstart[i]:wend[i]] <- stats::filter(
#       impulse_heat[wstart[i]:wend[i]],
#       filter = c(1 - lambda),
#       method = "recursive",
#       init = pred_temp_v[wstart[i]]
#    )

      # manual tail-call optimisation of an exponential smoothing filter
      # with variable lambda.  R's recursion is very resource-intensive and has
      # a very limited depth, so a manual unrolling is required; and the
      # loop-carried dependency makes this filter unvectorisable.
      prevpred <- pred_temp_v[wstart[i]]
      for (j in seq(wstart[i] + 1, wend[i])) {
        # a scalar inner loop, ouch!
        nextpred <- prevpred * EMA_param_complement[j] +
          heat_in_v[j] +
          ambient_v[j] * EMA_param[j]
        pred_temp_v[j] <- nextpred
        prevpred <- nextpred  # loop-carried dependency
      }

    }

    # we mask unpredicted temps with NA, to avoid skewing statistics of fit
    for (i in seq(nsegments)[which(segexclude)]) {
      pred_temp_v[wstart[i]:wend[i]] <- NA
    }

    # return to the tidyverse!  Hooray!!
    logtibble <- logtibble |>
      mutate(pred_pack_avg_temp = pred_temp_v,
             err_pred = pred_pack_avg_temp - pack_avg_temp,
             est_Joule_heating_kWh = cumsum(
               replace_na(est_Joule_heating, 0)) / 3.6e6, # in kWh
             est_entropic_heating_kWh = cumsum(
               replace_na(est_entropic_heating, 0)) / 3.6e6,
             est_polarisation_heating_kWh = cumsum(
               replace_na(est_heating_rp, 0) +
                 replace_na(est_heating_r2, 0)) / 3.6e6,
             AC_energy_kWh = cumsum(
               replace_na(heat_pump_cooling, 0)) / 3.6e6,
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
        interval_Joule_heating_kWh <-
          logtibble$est_Joule_heating_kWh[[to_idx]] -
          logtibble$est_Joule_heating_kWh[[from_idx]]
        interval_polarisation_heating_kWh <-
          logtibble$est_polarisation_heating_kWh[[to_idx]] -
          logtibble$est_polarisation_heating_kWh[[from_idx]]
        interval_entropic_heating_kWh <-
          logtibble$est_entropic_heating_kWh[[to_idx]] -
          logtibble$est_entropic_heating_kWh[[from_idx]]
        # n.b. my csv files are strongly biased toward fastcharging sessions,
        # strongly biasing the cumsum of entropic heating
        interval_total_heating <- interval_Joule_heating_kWh +
          interval_polarisation_heating_kWh +
          interval_entropic_heating_kWh
        cat(paste0("Total heating in window = ",
                   round(interval_total_heating, 0),
            " kWh. ",
            round(interval_Joule_heating_kWh /
                    interval_total_heating * 100, 3),
            "% Joule heat + ",
            round(interval_entropic_heating_kWh /
                    interval_total_heating * 100, 3),
            "% net entropic heat + ",
            round(interval_polarisation_heating_kWh /
                    interval_total_heating * 100, 3),
            "% polarisation heat\n"))
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
            ")\n")
        interval_Joule_heating_kWh <-
          logtibble$est_Joule_heating_kWh[[to_idx]] -
          logtibble$est_Joule_heating_kWh[[from_idx]]
        interval_polarisation_heating_kWh <-
          logtibble$est_polarisation_heating_kWh[[to_idx]] -
          logtibble$est_polarisation_heating_kWh[[from_idx]]
        interval_entropic_heating_kWh <-
          logtibble$est_entropic_heating_kWh[[to_idx]] -
          logtibble$est_entropic_heating_kWh[[from_idx]]
        interval_total_heating <- interval_Joule_heating_kWh +
          interval_polarisation_heating_kWh +
          interval_entropic_heating_kWh
        cat(paste0("Total heating = ", round(interval_total_heating, 0),
            " kWh. ",
            round(interval_Joule_heating_kWh /
                    interval_total_heating * 100, 1),
            "% Joule heat + ",
            round(interval_entropic_heating_kWh /
                    interval_total_heating * 100, 1),
            "% entropic heat + ",
            round(interval_polarisation_heating_kWh /
                    interval_total_heating * 100, 1),
            "% polarisation heat\n"))
        # n.b. the estimated COP of the heatpump is irrelevant, as we report
        # here only the estimated power consumption of the active cooling
        # system during charging sessions.
        # TODO: refactor this reportage into a separate function, with
        # percentage breakdowns of heating sources; percentage breakdowns of
        # observed heat rejections (active and passive); and also breakdowns
        # of energy efficiency (charging energy, traction motor energy,
        # heatpump & fan energy, other 12V supply energy, and "waste heat".
      }
    }
  }

  if (trace > 1) {
    logvalid <- logtibble$segnum != 0
    GIDs <- logtibble$gids[logvalid]
    est_SOC <- logtibble$est_soc[logvalid]
    cat("SOH:\n")
    cat("lSOC - est_SOC:\n")
    lSOC <- logtibble$lSOC[logvalid]
    print(summary(lSOC - est_SOC))
    if (trace > 2) {
      plot(GIDs, lSOC - est_SOC, main = tmodel$name)
    }

  }

  if (trace > 0) {
    cat("Thermal MSE =", round(MSE_of_fit(m), 4), "\n")
  }

  return(m)

}
