#' revise_ocv: refine an ocv_tbl, using predictions from predict_temp().
#' Displays a scatterplot, if trace > S0.  n.b. the ocv_tbl is used to
#' estimate entropic (reversible) heat generation, by lookup from the
#' LeafSpy-reported soc.  This soc is quite different to the cell-manufacturer's
#' soc, because it has a 0 at the vehicle-turtle level of "reserve" capacity
#' in the pack, rather than at the lower limit of the cell operating voltage.
#' This results in a very steep rise in OCV at small values of SOC in an
#' ocv_tbl (default: from the cell manufacturer) that is revised to align
#' with an soc that is estimated by a BMS in a Nissan EV.
#'
#' @examples
#' m <- revise_ocv(eNV50kWh)
#'
#' @param m a thmodel
#' @param wonky_threshold in volts, outlier criterion (default 70)
#' @param max_C max C-rate for OCV estimations
#' @param from_temp lower limit of battery temps to be analysed
#' @param to_temp upper limit of battery temps to be analysed
#' @param from_soc lower limit of SOC (in 0-1) to be analysed
#' @param to_soc upper limit of SOC to be analysed
#' @param tbl_size number of SOC values in the lookup table
#' @param methodology: "isoreg", "loess", "loess-sym", cir"
#' @param show_gids T to colour points by GIDs, F to colour by abs(pack_amps)
#' @param trace 2 for silent, 1 for minimal, 2 for verbose
#'
#' @returns a thmodel with an updated ocv_tbl and predictions
#' @export
#'
#' @examples
#' m <- revise_ocv(eNV50kWh)
revise_ocv <- function(m,
                    wonky_threshold = 70,
                    max_C = 0.4,
                    from_temp = NULL,
                    to_temp = NULL,
                    from_soc = NULL,
                    to_soc = NULL,
                    tbl_size = 51,
                    methodology = "loess",
                    show_gids = T,
                    trace = 1)
{

  stopifnot(class(m) == "thmodel")
  ot <- m$parameters$ocv_tbl
  ld <- m$logdata
  stopifnot(is.element("est_ocv_from_overvoltage", names(ld)))
  # n.b. the current version of predict_temp() computes this estimate from
  # pack_volts, pack_amps, the Tafel parameters, estimated temperature and
  # packr.  If a thmodel lacks this column in its logdata, then it may have
  # been created by a prior version of this package and lack a full set
  # of parameters for the current version.

  wonky <- (abs(ld$pack_volts -350) > wonky_threshold)
  missings <- is.na(ld$est_ocv_from_overvoltage) | is.na(ld$ssoc)
  unreliable <- ld$segnum == 0 # short sequences
  high_amps <- abs(ld$pack_amps) > max_C * m$capacity / 0.375
  min_temp <- if (is.null(from_temp)) -30 else from_temp
  max_temp <- if (is.null(to_temp)) 100 else to_temp
  extreme_temps <- (ld$pack_avg_temp < min_temp) | (ld$pack_avg_temp > max_temp)
  min_soc <- if (is.null(from_soc)) 0.0 else from_soc
  max_soc <- if (is.null(to_soc)) 1.0 else to_soc
  extreme_est_socs <- (ld$est_ssoc < min_soc) | (ld$est_ssoc > max_soc)

  if (trace > 0) {
    cat("est_ocv: filtering out",
        sum(high_amps, na.rm = TRUE), "high-amp records,",
        sum(extreme_temps, na.rm = TRUE), "extreme-temp records,",
        sum(extreme_est_socs, na.rm = TRUE), "extreme-est-soc records,",
        sum(missings, na.rm = TRUE), "incomplete records,",
        sum(unreliable, na.rm = TRUE), "unreliable records, and",
        sum(wonky, na.rm = TRUE), "wonky lines of data\n")
  }
  ld <- ld |> filter_out(high_amps |
                           extreme_temps |
                           extreme_est_socs |
                           missings |
                           unreliable |
                           wonky)
  if (trace > 0) {
    cat(" total remaining records:", nrow(ld), "\n")
  }
  stopifnot(nrow(ld) > 0)

  sloper <- (m$parameters$packr85 - m$parameters$effective_pack_resistance) / 15

  ld <- ld |>
    mutate(
      delta_t = date_time - dplyr::lag(date_time),
      eff_packr_2 = ifelse(
        ssoc <= 0.70,
        m$parameters$effective_pack_resistance,
        m$parameters$effective_pack_resistance + sloper * (ssoc - 0.70)
      ) *
        exp(m$parameters$arrhenius_resistance *
              (1 / 298.15 - 1 / (pack_avg_temp + 273.15))) / (hx / 100),
      #n.b. we use measured temp and hx, not predictions as in predict_temp
      tafel_slope_adj_2 = m$parameters$tafel_slope *
        exp(m$parameters$arrhenius_tafel *
              ((1 / 298.15) - (1 / (pack_avg_temp + 273.15))))
      #TODO: explore the sensitivity of tafel_slope_adj to hx
    )

  # we use segments as defined in predict_temp()
  for (i in unique(ld$segnum)) {
    smask <- (ld$segnum == i)
    ia <- ld$pack_amps[smask] # instantaneous amperage
    ts <- ld$tafel_slope_adj_2
    pi <- m$parameters$polarisation_irr
    iv <- -sign(ia) * exp(ts * log(abs(ia))) * (pi / 1000)
    # n.b. resistances are in mOhms.  pack_amps is negative when charging.
    # iv is a cathodic overvoltage (eta) estimated by a Tafel model

    # TODO: add an ohmic (charge-transfer) estimation for small
    # abs(ia), perhaps scaled from packr, to the time-averaged response?

    # TODO: are overvoltages diffusion-limited at large abs(ia)?  See
    # https://doi.org/10.1021/acs.jpcc.9b06820
    lambda <- ld$EMA_parameter_polarisation[[first(which(smask))]] # per-segment
    ov <- stats::filter(
      iv, # impulse voltages
      filter = c(1 - lambda), # Poisson's recursive exponential window function
      method = "recursive",
      init = iv[[1]]
    )
    ld$est_overvoltage_2[smask] <- ov
  }

  ld <- ld |>
    group_by(segnum) |>
    mutate(
      est_ocv_from_overvoltage_2 = pack_volts - est_overvoltage_2 +
        pack_amps * eff_packr_2 / 1000
      # TODO: we assume our Joule-heating parameter has a very short time
      # constant in its ohmic effect on the pack voltage.  Is this valid?
    ) |>
    arrange(ssoc, est_ocv_from_overvoltage_2)

  min_Hx <- round(min(ld$hx), 0)
  max_Hx <- round(max(ld$hx), 0)

  if (trace > 0) {
    cat(
      paste0(
        m$name,
        ": Hx = (",
        min_Hx,
        ", ",
        max_Hx,
        ")\n"
      )
    )
  }

  #enforce monotonicity in ocv table, using a least-squares fit
  ir <- isoreg(x = ld$ssoc, y = ld$est_ocv_from_overvoltage_2)
  if (trace > 1) plot(ir, xlab = "SOC", ylab = "OCV")

  # build new ocv_tbl, retaining extremal values from the current ocv_tbl
  newt <- tibble(SOC = ir$x[ir$iKnots],
                 OCV = ir$yf[ir$iKnots])
  otlo <- filter(ot,
                 SOC < ir$x[[dplyr::first(ir$iKnots)]],
                 OCV < ir$yf[[dplyr::first(ir$iKnots)]])
  othi <- filter(ot,
                 SOC > ir$x[[dplyr::last(ir$iKnots)]],
                 OCV > ir$yf[[dplyr::last(ir$iKnots)]])
  newt <- rbind(otlo, newt, othi)

  # sanity check on SOC values
  # n.b. the SOC in a LeafSpy log is an estimate.  I doubt it's ever > 100%. Its
  # lower limit seems to be somewhat above 10% (as per a cell-manufacturer's
  # datasheet), providing a safety margin for a turtled pack not becoming
  # bricked through self-discharge before it is trickle-charged back onto its
  # feet.
  stopifnot(min(newt[,"SOC"]) >= 0 || max(newt[,"SOC"]) <= 1.0)

  # convert newt into a table mapping SOC -> OCV, for use in approxfun()
  newt <- newt |>
    group_by(SOC) |>
    summarise(OCV = mean(OCV)) |>
    ungroup() |>
    arrange(SOC)

  #run isoreg "in the other direction", to estimate its bias
  ld <- ld |> mutate(
    sod = 1 - ssoc # "state of discharge"
  ) |>
    arrange(sod)
  ir2 <- isoreg(x = ld$sod, y = - ld$est_ocv_from_overvoltage_2)
  if (trace > 1) plot(ir2, xlab = "SOD", ylab = "-OCV")

  # build new ocv_tbl, retaining extremal values from the current ocv_tbl
  newt2 <- tibble(SOC = 1 - ir2$x[ir2$iKnots],
                  OCV = - ir2$yf[ir2$iKnots])
  otlo2 <- dplyr::filter(ot,
                  SOC < 1 - ir2$x[[last(ir2$iKnots)]],
                  OCV < - ir2$yf[[last(ir2$iKnots)]])
  othi2 <- dplyr::filter(ot,
                  SOC > 1 - ir2$x[[dplyr::first(ir2$iKnots)]],
                  OCV > - ir2$yf[[dplyr::first(ir2$iKnots)]])
  newt2 <- rbind(otlo2, newt2, othi2)

  # sanity check on SOC values
  stopifnot(min(newt2[,"SOC"]) >= 0 || max(newt2[,"SOC"]) <= 1.0)

  # convert newt2 into a table mapping SOC -> OCV, for use in approxfun()
  newt2 <- newt2 |>
    group_by(SOC) |>
    summarise(OCV = mean(OCV)) |>
    ungroup() |>
    arrange(SOC)

  # compute the mean of ocv predictions from newt and newt2
  otf <- approxfun(newt, method = "linear", rule = 2)
  otf2 <- approxfun(newt2, method = "linear", rule = 2)
  newt3 <- tibble(SOC=c(0:(tbl_size - 1)) / rep(tbl_size - 1, tbl_size)) |>
    rowwise() |>
    mutate(OCV = mean(otf(SOC), otf2(SOC)))
  if (trace > 1) {
    cat("Isoreg-predicted OCVs: ", newt3$OCV, "\n")
  }
  # let's try loess()
  ld <- ld |> mutate(
    jsoc = jitter(ssoc, 0.0001)) # jitter to avoid ties
  if (methodology == "loess-sym") {
    # this may produce a better fit if there are outliers
    otf4 <- loess(ld$est_ocv_from_overvoltage_2 ~ jsoc,
                  ld, family = "symmetric")
  } else {
    otf4 <- loess(ld$est_ocv_from_overvoltage_2 ~ jsoc, ld)
  }
  socv <- c(0:(tbl_size - 1)) / rep(tbl_size - 1, tbl_size)
  predv <- predict(otf4, data.frame(jsoc = socv))
  predv <-
    tibble(OCV = predv, SOC = socv) |>
    mutate(preddv = OCV - dplyr::lag(OCV))
  newt4 <- predv |>
    filter_out(is.na(OCV)) |>
    select(!preddv)
  otlo4 <- dplyr::filter(ot, SOC < min(newt4$SOC), OCV < min(newt4$OCV))
  othi4 <- dplyr::filter(ot, SOC > max(newt4$SOC), OCV > max(newt4$OCV))
  newt4 <- rbind(otlo4, newt4, othi4)
  if (trace > 1) {
    cat("Loess-predicted OCVs:", newt4$OCV, "\n")
  }
  if ((methodology == "loess") && (min(predv$preddv, na.rm = TRUE) < 0)) {
    warning("loess() of estimated OCVs is not monotone increasing")
  }

  # now let's try centred isotonic regression
  if (methodology == "cir") {
    # the following throws a mysterious error -- possibly because it
    # can't handle this many points?
    dr <- cir::doseResponse(x = ld$jsoc, y = ld$est_ocv_from_overvoltage_2)
    ir5 <- cir::cirPAVA(dr)
    if (trace > 1) plot(ir5, xlab = "SOC", ylab = "OCV")
    newt5 <- tibble(SOC = ir5$x,
                    OCV = ir5$y)
    otlo5 <- filter(ot,
                    SOC < dplyr::first(ir5$x),
                    OCV < dplyr::first(ir5$y))
    othi5 <- filter(ot,
                    SOC > dplyr::last(ir5$x),
                    OCV > dplyr::last(ir5$x))
    newt5 <- rbind(otlo5, newt5, othi5)
  }

  # visualise
  if (trace > 0) {
    min_Hx <- round(min(ld$hx), 0)
    max_Hx <- round(max(ld$hx), 0)
    min_SOH <- round(min(ld$soh), 0)
    max_SOH <- round(max(ld$soh), 0)
    ld <- ld |> arrange(ld$date_time)
    min_date <- floor_date(
      as.POSIXct(dplyr::first(ld$date_time), tz = "UTC"), "day")
    max_date <- floor_date(
      as.POSIXct(dplyr::last(ld$date_time), tz = "UTC"), "day")
    newtf <- newt |>   filter(SOC >= min_soc, SOC <= max_soc)
    newt2f <- newt2 |> filter(SOC >= min_soc, SOC <= max_soc)
    newt3f <- newt3 |> filter(SOC >= min_soc, SOC <= max_soc)
    newt4f <- newt4 |> filter(SOC >= min_soc, SOC <= max_soc)
    if (methodology == "cir") {
      newt5f <- newt5 |> filter(SOC >= min_soc, SOC <= max_soc)
    }
    e <-
      ggplot() +
      geom_line(data = newtf, aes(x = SOC, y = OCV), colour = "orange") +
      geom_line(data = newt2f, aes(x = SOC, y = OCV), colour = "red") +
      geom_line(data = newt3f, aes(x = SOC, y = OCV), colour = "green") +
      geom_line(data = newt4f, aes(x = SOC, y = OCV), colour = "blue") +
      if (show_gids)
        geom_point(data = ld, aes(
          x = ssoc,
          y = pack_volts,
          colour = gids
        ))
    else
      geom_point(data = ld, aes(
        x = ssoc,
        y = pack_volts,
        colour = abs(pack_amps)
      ))
    if (methodology == "cir") {
      e <- e +
        geom_line(data = newt5f, aes(x = SOC, y = OCV), colour = "violet")
    }

    plot(e +
      theme(palette.colour.continuous = "Okabe-Ito") +
      labs(,
        x = "SOC",
        y = "Pack Volts",
        title = paste0(
          m$name,
          ": ", min_date,
          " to ", max_date,
          ifelse(is.null(from_temp) && is.null(to_temp),
                 "",
                 paste0(
                   ifelse(is.null(from_temp),
                          ", temp",
                          paste0(", ", from_temp, " ≤ temp")),
                   ifelse(is.null(to_temp),
                          "",
                          paste0(" ≤ ", to_temp))
                 )),
          ifelse(is.null(from_soc) && is.null(to_soc),
                 "",
                 paste0(
                   ifelse(is.null(from_soc),
                          ", soc",
                          paste0(", ", from_soc, " ≤ soc")),
                   ifelse(is.null(to_soc),
                          "",
                          paste0(" ≤ ", to_soc))
                 )),
          paste0(", C ≤ ", max_C),
          ". Hx = (", min_Hx,
          ", ", max_Hx, ")",
          ", SOH = (", min_SOH,
          ", ", max_SOH, ")"
        )
      )
    )
  }

  m$ocv_tbl <- if (methodology == "isoreg") newt3 else
    if (methodology == "cir") newt5 else newt4

  # retval: a thmodel with recomputed predictions
  predict_temp(m, trace = trace)

}
