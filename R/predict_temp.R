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
#' cSOC is computed from gids.  It has a value that is offset from lSOC in the
#' 50kWh pack by approximately 0.14 (when SOH=100), to account for the
#' gids_reserve in the pack at lSOC == 0.0, and with a scaling of lSOC by about
#' 0.86 so that cSOC == lSOC == 1.0 at the maximum operating voltage for a cell
#' (as disclosed by its manufacturer's spec sheet) and at the soc == 1e6 maximum
#' value for this LeafSpy-reported estimate of the pack's available energy.
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
#' @param ocv_tbl maps cSOC onto OCV, a 2-column tibble
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
  # monotonically-increasing values of OCV with respect to cSOC).  I recommend
  # caution when using this routine, because of the danger of overfitting
  # ocv_tbl to non-representative extremal sequences of LeafSpy readouts.  Note:
  # the LeafSpy-reported lSOC has a zero when the cells still have some reserve
  # capacity; and the dSOC (reported on the dashboard to the driver) has
  # yet-another scaling.   The "reserve" remaining at lSOC == 0 is yet-another
  # parameter in the model, by default set to 10% of the nominal kWh capacity of
  # the pack when its SOH == 1.
  #
  # TODO: use our (crude) voltage-modelling to estimate cSOC independently
  # of the LeafSpy-reported soc.  I have misgivings about the accuracy of
  # the soc estimations of the VIVNE-supplied firmware, when lSOC is below 0.5.
  # An independent estimate of cSOC may shed some light on any bias or
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
               ", λt1 = ", round(lambda1_polarisation, 5), # RpC1 time-constant
               ", λt2 = ", round(lambda2_polarisation, 5), # R2C2 time-constant
               ", fanp = ", round(fan_power, 5),
               ", COP = ", round(COP, 5),
               ", aR = ", round(arrhenius_resistance, 5),
               ", aR2 = ", round(arrhenius_R2, 5),
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

  if (polarisation_Rp > effective_pack_resistance - 1) {
    polarisation_Rp <- effective_pack_resistance - 1
    warning("Setting R1 to 1 mΩ, by reducing an excessively-large Rp",
            "to Reff - 1\n")
  }

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
  # (m^2 / 2)R to the estimated Joule heating. I estimate the slope at the
  # midpoint of each sampling interval using a first-order forward divided
  # difference.  This is NA at the end of each segment, which I adjust to 0 for
  # convenience when applying this additive correction to a numeric integration.
  # n.b. delta_t is NA in cases where sampling has been interrupted (as
  # indicated by a long interval between timestamps).  The data is noisy
  # enough, and its apparent slope is variable enough, that higher-order
  # estimates of slope seem very unlikely to reduce bias without greatly
  # degrading the accuracy of my numerical integration of Joule heating.
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
              (lambda_module_AC_to_ambient * 3600)), # parameterised in hours
      EMA_parameter_p1 =
        min(1.0, sampling_interval / lambda1_polarisation), # seconds
      EMA_parameter_p2 =
        min(1.0, sampling_interval / lambda2_polarisation), # seconds
      # n.b. the polarisation time constants are adjusted below for pack_temp

      .before = pack_avg_temp
    ) |>
    ungroup()

  kWh_reserve <- gids_reserve * 0.080
  SOC_reserve <- kWh_reserve / m$capacity
  SOC_usable <- 1 - SOC_reserve
  logtibble <- logtibble |>
    mutate(
      lSOC = soc / 1e6, # scale this 20-bit integer to a 0-1 range
      cSOC = gids * 0.080 / (m$capacity * soh / 100),
      # cSOC is a "pure" SOC with no reserve, so that my estimated-OCV tables
      # are directly comparable to published results for cells (albeit with
      # differences in exactly how the "capacity" is defined -- which may be
      # relative to a 0.2C charge/discharge table in the OEM firmware for the
      # LeafSpy BMS)
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

      est_ocv_from_cSOC = f_soc_to_ocv(cSOC),

      est_ocv_from_pack_behaviour = est_ocv_from_cSOC,
      # initial estimate, will be refined through simulation

      est_Joule_heating_amperage = NA,
      # pack charging/discharging current, as estimated from an equivalent
      # circuit (R1 - (Rp // C1)) // (R2 // C2). See Ramón 2022.
      est_polarisation_amperage_1 = NA,
      est_polarisation_amperage_p = NA,
      # current through R1 (resp. Rp) attributed to a discharge or charge of C1
      est_polarisation_amperage_2 = NA,
      # current through R2 in the R2 // C2 branch. This component of
      # polarisation heating has a short time constant, and is intended to
      # model the polarisation of a graphite anode.
      # n.b. there's a hazard in a nonlinear fit if the initial value of
      # lambda2 == R2*C2 is not much smaller than lambda1
      unattributed_amperage = NA,
      # violations of Kirchhoff's Law of Current at the junction of R1 and R2.
      # An analysis of this time-series may be of diagnostic value when
      # estimating an ocv_tbl for a pack at its current Hx and SOH,
      # when estimating a bias in the LeafSpy-reported SOC, or when adjusting
      # the lumped elements (R1, Rp, R2, C1, C2) in the equivalent circuit.

      .before = pack_avg_temp

    )

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
    # I also rely on the LeafSpy-reported cSOC and the ocv_tbl for an initial
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
    #
    # TODO: consider writing a more accurate electrical simulation for long
    # segments with large temperature shifts.  This would most easily be done
    # in C, using an optimising compiler to produce code that's efficiently
    # executed on a superscalar CPU without going to the trouble of
    # expressing an EMA with a vector-valued lambda as a loop with a
    # loop-carried dependency, rather than naturally as a tail recursion.

    logtibble$pred_pack_avg_temp[wstart[!segexclude]] <-
      logtibble$pack_avg_temp[wstart[!segexclude]]
    # n.b. the code makes no predictions of the pack temp at the beginning of
    # each segment, however as a programming convenience for the Arrhenius
    # adjustments I make a "prediction" at these sampling times.  This
    # "prediction" is set to NA at the end of each iteration, so that MSE
    # can be computed easily without a downward bias.

    logtibble <- logtibble |>
      group_by(segnum) |>
      mutate(
        eff_packr = effective_pack_resistance *
          exp(arrhenius_resistance *
                (1 / 298.15 - 1 / (pred_pack_avg_temp + 273.15))) /
          (pred_hx / 100),
        rp_tc = polarisation_Rp *
          exp(arrhenius_resistance *
                (1 / 298.15 - 1 / (pred_pack_avg_temp + 273.15))) /
          (pred_hx / 100),
        # TODO: validate the temperature-corrected Rp using the same Arrhenius
        # constant for Rp as for the series combination R1 - Rp == Reff.  For
        # adequate accuracy, it may be necessary to have an explicit R1
        # parameter with its own temperature compensation, and to compute Reff
        # as the sum of R1 and Rp. However this would make it more difficult to
        # drop back to a simple 1-resistor EC; at present this can be done
        # simply, by setting the two polarisation time-constants to 0.
        r1_tc = eff_packr - rp_tc,
        r2_tc = polarisation_R2 *
          exp(arrhenius_R2 *
                (1 / 298.15 - 1 / (pred_pack_avg_temp + 273.15))) /
          (pred_hx / 100),
        # TODO: is arrhenius_R2 important to the accuracy of our model?  Seems
        # fiddly... and having yet-another parameter is opening yet-another door
        # to overfitting, as well as wasting resource in a non-linear
        # optimisation and confusing the reportage of a best-fit with fiddly
        # adjustments of dubious validity to the value of a weakly-fitted
        # parameter.
        lambda1 = EMA_parameter_p1 *
          exp(arrhenius_resistance *
                ((1 / 298.15) - (1 / (pred_pack_avg_temp + 273.15)))),
        lambda2 = EMA_parameter_p2 *
          exp(arrhenius_R2 *
                ((1 / 298.15) - (1 / (pred_pack_avg_temp + 273.15))))
      ) |>
      ungroup()

    for (i in seq(nsegments)[which(!segexclude)]) {
      ia <- logtibble$pack_amps[wstart[i]:wend[i]] # instantaneous amperage
      iv <- logtibble$pack_volts[wstart[i]:wend[i]] # instantaneous voltage
      idv <- replace_na(iv - dplyr::lag(iv), 0) # instantaneous delta voltage
      reff <- logtibble$eff_packr[wstart[i]:wend[i]] / 1000 # in Ohms
      r1 <- logtibble$r1_tc[wstart[i]:wend[i]] / 1000
      r2 <- logtibble$r2_tc[wstart[i]:wend[i]] / 1000
      rp <- logtibble$rp_tc[wstart[i]:wend[i]] / 1000
      l1 <- mean(logtibble$lambda1[wstart[i]:wend[i]], na.rm=T)
      l2 <- mean(logtibble$lambda2[wstart[i]:wend[i]], na.rm=T)

      ocv <- logtibble$est_ocv_from_pack_behaviour[wstart[i]:wend[i]]
      overv <- iv - ocv
      # current estimate of overvoltage

      iac <- ia
      # estimate of charging current (through r1-rp)
      # TODO: find a reliable way to estimate the overvoltage, so that
      #     iac <- - overv / reff
      # This will depend critically on the accuracy of entries in ocv_tbl
      # and on the accuracy of the SOC estimate.  At present we attribute all
      # of ia to iac, but this violates Kirchhoff's Law whenever there are
      # non-negligible polarisation currents.  Furthermore, we assume the
      # voltages across the capacitors C1 and C2 are always at steady state
      # when we attribute idv to the polarisation currents, even though iac may
      # change rapidly as a result of motor activity and the time-constants
      # on polarisation are significantly longer than the sampling interval.
      # One possibility for improvement would be to estimate time-series
      # for V1 (across C1) and V2 (across C2), using these to adjust the
      # way in which ia1, ia2, and iap are estimated from idv.  Another
      # possibility would be to use motor draw and charging power in an
      # estimation of systemic bias in iac, soc, and ocv_tbl.

      ia1 <- - as.vector(stats::filter(l1 * idv / r1,
                           filter = c(1 - l1),
                           method = "recursive",
                           # exponential decay with time constant l1
                           init = idv[1] / r1[1]
      )) # polarisation current through r1 via c1
      # n.b. setting l1 to 0 results in a zero estimate for ia1, allowing
      # a fallback to a simpler model with no capacitance across r1.

      # TODO: compare ia1 with a more accurate simulation, in which lambda1
      # varies over a segment as the pack_temp changes.  Regrettably, such a
      # time-variant filter is outside the scope of the C-coded stats::filter(),
      # so this would require a loop for each segment i with an iteration count
      # of (wend[i] - wstart[i] - 1) -- as we have done below for the thermal
      # simulation.

      iap <- - as.vector(stats::filter(l1 * idv / reff,
                                     filter = c(1 - l1),
                                     method = "recursive",
                                     init = idv[1] / reff[1]
      )) # polarisation current through r1 via rp
      # n.b. the voltage drop across r1 is (iac + ia1 + iap)*r1, and
      # the voltage drop across rp is (iac + iap)*rp

      ia2 <- - as.vector(stats::filter(l2 * idv / r2,
                           filter = c(1 - l2),
                           method = "recursive",
                           init = idv[1] / r2[1]
      )) # polarisation current through r2

      iau <- ia - iac - ia1 - iap - ia2
      # unattributed current, indicative of error in the simulation

      ocv_prime <- iv +
        (iac + ia1 + iap) * r1 +
        (iac + iap) * rp +
        ia2 * r2
      # revised estimate of ocv
      # n.b. pack voltage is depressed by a discharge (positive sign on ia)

      # TODO: find a reliable estimation of an iac_prime, ia1_prime, ia2_prime,
      # and iap_prime from estimations of errors in the electrical simulation
      # e.g. Kirchhoff's Law violations at the junction of R1 and Rp,
      # estimations of non-steady-state voltages across C1 and C2, or
      # coulomb-counting on iac (scaled by pack usable capacity) vs
      # LeafSpy-reported changes in SOC.

      if (any(abs(iau) > 1000)) {
        # n.b. excessive violations of Kirchhoff's Law are indicative of a
        # highly-inaccurate simulation, perhaps worthy of an immediate
        # investigation
        diagt <- tibble(ia, iv, idv, ocv, ocv_prime,
                        reff, r1, rp, r2,
                        iac, ia1, iap, ia2, iau)
        write.csv(diagt, paste0("seg", i, ".", iternum, ".csv"))
        browser()
      }
      # TODO: remove this diagnostic code prior to a release, but perhaps stop
      # if excessive violations of Kirchhoff's Law are detected?
      #     stopifnot(all(abs(ia - iac - ia1 - iap - ia2) < 1000))

      # TODO: see if prediction errors correlate with the sign of ia.  I'm
      # assuming the same equivalent circuit for both charging and discharging,
      # but this assumption must be validated.

      if (i == 913) { # this is an interesting segment in env50kWh.rda
        diagt <- tibble(ia, iv, idv, ocv, ocv_prime,
                        reff, r1, rp, r2,
                        iac, ia1, iap, ia2, iau)
        write.csv(diagt, paste0("seg913.", iternum, ".csv"))
      }

      logtibble$est_Joule_heating_amperage[wstart[i]:wend[i]] <- iac
      # charging/discharging amperage of traction pack
      logtibble$est_polarisation_amperage_1[wstart[i]:wend[i]] <- ia1
      logtibble$est_polarisation_amperage_p[wstart[i]:wend[i]] <- iap
      # polarisation amperages with time constant lambda1
      logtibble$est_polarisation_amperage_2[wstart[i]:wend[i]] <- ia2
      # polarisation amperage through r2, with time constant lambda2
      logtibble$est_ocv_from_pack_behaviour[wstart[i]:wend[i]] <- ocv_prime
      # adjusted estimate of ocv
      logtibble$unattributed_amperage[wstart[i]:wend[i]] <- iau
      # unattributed amperage, indicative of inaccuracy in the elec model

    }

    # TODO: consider transmogrifying the code above into the tidyverse.
    # DONE: Not a good idea.
    # It's far from clear to me whether the tidyverse will support grouped
    # calls to stats::filter() without workarounds.  More importantly: the
    # code above is more amenable to modification, e.g. as a halfway-house to
    # a more accurate simulation with lambda that vary within an interval.
    # Such a simulation is neither vectorisable nor supported by
    # stats::filter(), but it could be tidily expressed in a language (e.g. C)
    # with an optimising compiler capable of performing loop unrolling (for
    # efficient execution on a superscalar CPU). Estimated cost: 1 SMOP.
    # Estimated ROI: negative.

    logtibble <- logtibble |>
      group_by(segnum) |>
      mutate(
        est_Joule_heating =
          delta_t *
          est_Joule_heating_amperage * est_Joule_heating_amperage *
          eff_packr / 1000,
        # in Ws.  n.b. resistances are in mOhms

        est_Joule_heating_slope_corr =
          delta_t *
          (est_Joule_heating_amperage * est_Joule_heating_amperage +
             slope_amps * slope_amps / 2) * eff_packr / 1000,
        # TODO: is this correction helpful, irrelevantly small, or badly skewed
        # due to short-term variations in polarisation currents dominating the
        # deltas in pack_amps? Maybe estimate the first derivative of all
        # identified currents? Maybe the short time constant of
        # est_polarisation_amperage_2 response to a shift in pack voltage makes
        # its first derivative tricky to estimate in an unbiased way?

        est_polarisation_heating_1 =
          delta_t *
          est_polarisation_amperage_1 * est_polarisation_amperage_1 *
          r1_tc / 1000,

        est_polarisation_heating_p =
          delta_t *
          est_polarisation_amperage_p * est_polarisation_amperage_p *
          eff_packr / 1000,

        est_polarisation_heating_2 =
          delta_t *
          est_polarisation_amperage_2 * est_polarisation_amperage_2 *
          r2_tc / 1000,

        est_soc = f_ocv_to_soc(est_ocv_from_pack_behaviour),

        est_entropic_heating =
          delta_t *
          pack_amps * (pack_avg_temp + 298.15) * entropic_heat / 1000,
        # in Ws
        # n.b. this is a reversible heat, causing the pack to heat somewhat
        # less when discharging at a given current than when charging at the
        # same rate.

        est_entropic_heating_alt =
          delta_t *
          pack_amps *
          (pack_avg_temp + 298.15) * (
            - 0.07 -
              graphite_entropy(cSOC) +
              entropic_heat) /
          1000, # in W-s
        # the entropy change of the graphite anode is roughly 0.07 mV/K
        # for SOC > 0.5, with an SOC-related variance of about 0.02 mV/K.
        # our "entropic heat" parameter is intended to be the central
        # tendency (to minimise the swing between the simple estimation
        # and the SOC-related ones)

        # n.b. the sign of pack_amps is reversed from the convention in
        # the technical literature

        # n.b. the entropy change of an NMC cathode is dependent on its
        # NMC ratios, its doping, and the electrolyte; and may be quite
        # large for low SOC.

        # TODO: analyse low-SOC segments in the LeafSpy traces, to see if
        # their thermal evolution might be confidently attributed to an
        # extraordinarily-large entropic heating or cooling.

        est_entropic_heating_alt_alt =
          delta_t *
          pack_amps *
          (pack_avg_temp + 298.15) * (
            - 0.07 -
              graphite_entropy(est_soc) +
              entropic_heat) /
          1000,
        # in W-s

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
                          est_polarisation_heating_1 +
                          est_polarisation_heating_p +
                          est_polarisation_heating_2 -
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
    # module to ambient (in units of the sampling_interval, rather than seconds
    # or hours).
    for (i in seq(nsegments)[which(!segexclude)]) {

#    lambda <- EMA_param[[wstart[i]]] # per-segment!
     # if lambda were constant over a segment, stats::filter() would be
     # elegant and efficiently-executed on any superscalar CPU:
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
               replace_na(est_polarisation_heating_1, 0) +
                 replace_na(est_polarisation_heating_2, 0) +
                 replace_na(est_polarisation_heating_p, 0)) / 3.6e6,
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
        interval_total_heating <- interval_Joule_heating_kWh +
          interval_polarisation_heating_kWh +
          interval_entropic_heating_kWh
        interval_mean_unattributed_amperage <-
          mean(logtibble$unattributed_amperage[from_idx:to_idx], na.rm = T)
        interval_meansq_unattributed_amperage <-
          mean(logtibble$unattributed_amperage[from_idx:to_idx] *
                 logtibble$unattributed_amperage[from_idx:to_idx],
               na.rm = T)
        cat(paste0("Total heating =", round(interval_total_heating, 0),
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
        cat("Mean unattributed amperage:",
            round(interval_mean_unattributed_amperage, 1),
            "; Mean(iau*iau):",
            round(interval_meansq_unattributed_amperage, 1),
            "\n"
        )
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
        interval_mean_unattributed_amperage <-
          mean(logtibble$unattributed_amperage[from_idx:to_idx], na.rm = T)
        interval_meansq_unattributed_amperage <-
          mean(logtibble$unattributed_amperage[from_idx:to_idx] *
                 logtibble$unattributed_amperage[from_idx:to_idx],
               na.rm = T)
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
        cat("Mean unattributed amperage:",
            round(interval_mean_unattributed_amperage, 1),
            "; Mean(iau*iau):",
            round(interval_meansq_unattributed_amperage, 1),
            "\n")
      }
    }
  }

  if (trace > 1) {
    logvalid <- logtibble$segnum != 0
    GIDs <- logtibble$gids[logvalid]
    est_SOC <- logtibble$est_soc[logvalid]
    cSOC <- logtibble$cSOC[logvalid]
    cat("SOH:\n")
    print(summary(logtibble$soh[logvalid]))
    cat("cSOC - est_SOC:\n")
    print(summary(cSOC - est_SOC))
    if (trace > 2) {
      plot(GIDs, cSOC - est_SOC, main = tmodel$name)
    }

  }

  if (trace > 0) {
    cat("Thermal MSE =", round(MSE_of_fit(m), 4), "\n")
  }

  return(m)

}
