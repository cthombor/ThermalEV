#' revise_ocv: refine an ocv_tbl, using predictions from predict_temp().
#' Displays a scatterplot, if trace > S0.
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
  m$parameters$prev_ocv_tbl <- ot
  ot <- mutate(ot,
               DVDJ = NULL,
               DVDC = NULL)
  ld <- m$logdata

  wonky <- (abs(ld$pack_volts -350) > wonky_threshold)
  missings <- is.na(ld$est_ocv_from_pack_behaviour) | is.na(ld$lSOC) |
    is.na(ld$eff_packr)
  unreliable <- ld$segnum == 0 # short sequences
  high_amps <- abs(ld$pack_amps) > max_C * m$capacity / 0.375
  min_temp <- if (is.null(from_temp)) -30 else from_temp
  max_temp <- if (is.null(to_temp)) 100 else to_temp
  extreme_temps <- (ld$pack_avg_temp < min_temp) | (ld$pack_avg_temp > max_temp)
  min_soc <- if (is.null(from_soc)) 0.0 else from_soc
  max_soc <- if (is.null(to_soc)) 1.0 else to_soc
  extreme_socs <- (ld$lSOC < min_soc) |
    (ld$lSOC > max_soc)

  if (trace > 0) {
    cat("revise_ocv: filtering out",
        sum(high_amps, na.rm = TRUE), "high-amp records,",
        sum(extreme_temps, na.rm = TRUE), "extreme-temp records,",
        sum(extreme_socs, na.rm = TRUE), "extreme-soc records,",
        sum(missings, na.rm = TRUE), "incomplete records,",
        sum(unreliable, na.rm = TRUE), "unreliable records, and",
        sum(wonky, na.rm = TRUE), "wonky lines of data\n")
  }
  ld <- ld |> filter_out(high_amps |
                           extreme_temps |
                           extreme_socs |
                           missings |
                           unreliable |
                           wonky)
  if (trace > 0) {
    cat(" total remaining records:", nrow(ld), "\n")
  }
  stopifnot(nrow(ld) > 0)

  ld <- ld |>
    mutate(
      delta_t = date_time - dplyr::lag(date_time),
      eff_packr_corr =
        m$parameters$effective_pack_resistance *
        exp(m$parameters$arrhenius_resistance *
              (1 / 298.15 - 1 / (pack_avg_temp + 273.15))) / (hx / 100),
      #n.b. we use the measured temp and hx, not predictions as in predict_temp
    )

  # we use segments as defined in predict_temp()
  ld <- ld |>
    group_by(segnum) |>
    mutate(
      est_ocv_from_pack_behaviour_2 = est_ocv_from_pack_behaviour -
        est_Joule_heating_amperage * (eff_packr_corr - eff_packr) / 1000
      # this should be a more accurate estimate, because it uses measured temps
    ) |>
    arrange(lSOC, est_ocv_from_pack_behaviour_2)

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
  ir1 <- stats::as.stepfun(
    stats::isoreg(
      x = ld$lSOC, y = ld$est_ocv_from_pack_behaviour_2))
  if (trace > 1) plot(ir1, xlab = "SOC", ylab = "OCV")

  # build new ocv_tbl, retaining extremal values from the current ocv_tbl
  newt <- tibble(SOC = knots(ir1),
                 OCV = ir1(SOC))
  otlo <- filter(ot,
                 SOC < dplyr::first(newt$SOC),
                 OCV < dplyr::first(newt$OCV))
  othi <- filter(ot,
                 SOC > dplyr::last(newt$SOC),
                 OCV > dplyr::last(newt$OCV))
  newt <- rbind(otlo, newt, othi)

  # sanity check on SOC values
  stopifnot(min(newt[,"SOC"]) >= 0.0 || max(newt[,"SOC"]) <= 1.0)
  stopifnot(min(newt$SOC - dplyr::lag(newt$SOC), na.rm = TRUE) > 0)
  stopifnot(min(newt$OCV - dplyr::lag(newt$OCV), na.rm = TRUE) >= 0)
  # n.b. the SOC in a LeafSpy log is an estimate.  I doubt it ever exceeds 100%,
  # but I'm programming defensively to prevent such out-of-band values creeping
  # into the ocv_tbl.  I also doubt the SOC will ever drop below 0%, as that
  # would (presumably) be an estimated OCV below the cell-manufacturer's lower
  # operating limit, likely to cause severe damage if the cell is sourcing any
  # current.  Indeed: the lower operating limit for SOC in a Nissan EV seems to
  # be 10%, which presumably is introduced as a safety margin to allow a turtled
  # EV to survive for at least a few days (despite self-discharge) before it is
  # put onto a resuscitating regime of a trickle-charge.

  #run isoreg "in the other direction", to estimate its bias
  ld <- ld |> mutate(
    sod = 1 - lSOC # "state of discharge"
  ) |>
    arrange(sod)
  ir2 <- stats::as.stepfun(
    stats::isoreg(x = ld$sod, y = - ld$est_ocv_from_pack_behaviour_2))

  newt2 <- tibble(SOC = 1 - knots(ir2),
                 OCV = - ir2(1 - SOC)) |>
    arrange(SOC)
  otlo <- filter(ot,
                 SOC < dplyr::first(newt2$SOC),
                 OCV < dplyr::first(newt2$OCV))
  othi <- filter(ot,
                 SOC > dplyr::last(newt2$SOC),
                 OCV > dplyr::last(newt2$OCV))
  newt2 <- rbind(otlo, newt2, othi)

  stopifnot(min(newt2[,"SOC"]) >= 0.0 || max(newt2[,"SOC"]) <= 1.0)
  stopifnot(min(newt2$SOC - dplyr::lag(newt2$SOC), na.rm = TRUE) > 0)
  stopifnot(min(newt2$OCV - dplyr::lag(newt2$OCV), na.rm = TRUE) >= 0)

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
    jsoc = jitter(lSOC, 0.0001)) # jitter to avoid ties
  if (methodology == "loess-sym") {
    # this may produce a better fit if there are outliers
    otf4 <- loess(ld$est_ocv_from_pack_behaviour_2 ~ jsoc,
                  ld, family = "symmetric")
  } else {
    otf4 <- loess(ld$est_ocv_from_pack_behaviour_2 ~ jsoc, ld)
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
    # TODO: repair this code, which throws a mysterious error -- possibly
    # because cir::doseResponse() can't handle this many points?
    dr <- cir::doseResponse(x = ld$jsoc, y = ld$est_ocv_from_pack_behaviour_2)
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
      if (show_gids)
        geom_point(data = ld, aes(
          x = lSOC,
          y = pack_volts,
          colour = gids
        ))
    else
      geom_point(data = ld, aes(
        x = lSOC,
        y = pack_volts,
        colour = abs(pack_amps)
      ))

    if (methodology == "cir") {
      e <- e +
        geom_line(data = newt5f, aes(x = SOC, y = OCV), colour = "violet")
    }

    plot(
      e +
        geom_line(data = ot, aes(x = SOC, y = OCV),colour = "black",
          linetype = "dashed") +
        geom_line(data = newtf, aes(x = SOC, y = OCV), colour = "orange") +
        geom_line(data = newt2f, aes(x = SOC, y = OCV), colour = "red") +
        geom_line(data = newt3f, aes(x = SOC, y = OCV), colour = "green") +
        geom_line(data = newt4f, aes(x = SOC, y = OCV), colour = "blue") +
        theme(palette.colour.continuous = "Okabe-Ito") +
      labs(,
        x = "SOC from LeafSpy",
        y = "Estimated OCV",
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

  ot <- if (methodology == "isoreg") newt3 else
    if (methodology == "cir") newt5 else newt4
  m$parameters$ocv_tbl <- add_dvdj_to_ocv_tbl(ot)

  # retval: a thmodel with recomputed predictions
  predict_temp(m, trace = trace)

}
