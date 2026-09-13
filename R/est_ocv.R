#' est_ocv: refine an ocv_tbl, from errors in an om_model's voltage prediction
#'
#' Usage notes: the new ocv_tbl can be inserted into a thmodel using
#' predict_temp, e.g.
#' predict_temp(eNV200ac50kWh, ocv_tbl=est_ocv(om_eNV50kWh)).
#' This should reduce the variance in the voltage predictions; but you may
#' also find it helpful to adjust the resistances using fit_r_to_ocv() before
#' running est_ocv(), e.g.
#' om <- fit_r_to_ocv(om_eNV50kWh)
#'
#' @param om an ocv_model
#' @param wonky_threshold in Volts, outlier criterion (default 50)
#' @param max_C max C-rate for OCV estimations
#' @param from_temp lower limit of battery temps to be analysed
#' @param to_temp upper limit of battery temps to be analysed
#' @param from_soc lower limit of SOC (in 0-1) to be analysed
#' @param to_soc upper limit of SOC to be analysed
#' @param tbl_size number of SOC values in the lookup table
#' @param methodology: "isoreg", "loess", "loess-sym", cir"
#' @param trace 0 for silent, 1 for minimal, 2 for verbose
#'
#' @returns an ocv_model with an updated ocv_tbl and voltage predictions
#' @export
#'
#' @examples
#' est_ocv(om_eNV50kWh)
est_ocv <- function(om,
                    wonky_threshold = 50,
                    max_C = 0.4,
                    from_temp = NULL,
                    to_temp = NULL,
                    from_soc = NULL,
                    to_soc = NULL,
                    tbl_size = 51,
                    methodology = "loess",
                    trace = 1)
{

  ot <- om$ocv_tbl
  ld <- om$logdata
  ld <- ld |>
    mutate(pred_error_pack_volts = pred_pack_volts - pack_volts,
           ocv_estimate = pack_volts - pack_amps * eff_packr / 1000,
           delta_t = date_time - dplyr::lag(date_time)) |>
    arrange(soc, ocv_estimate)

  wonky <- (ld$pred_error_pack_volts > wonky_threshold)
  missings <- is.na(ld$ocv_estimate) | is.na(ld$soc)
  starts <- ld$delta_t >= 120 # gap of two minutes or more
  ends <- lead(starts)
  singletons <- starts & ends # unreliable readings
  high_amps <- abs(ld$pack_amps) > max_C * om$capacity / 0.375
  min_temp <- if (is.null(from_temp)) -30 else from_temp
  max_temp <- if (is.null(to_temp)) 100 else to_temp
  extreme_temps <- (ld$pack_avg_temp < min_temp) | (ld$pack_avg_temp > max_temp)
  min_soc <- if (is.null(from_soc)) 0.0 else from_soc
  max_soc <- if (is.null(to_soc)) 1.0 else to_soc
  extreme_socs <- (ld$soc < min_soc) | (ld$soc > max_soc)

  if (trace > 0) {
    cat("est_ocv: filtering out",
        sum(high_amps, na.rm = TRUE), "high-amp records,",
        sum(extreme_temps, na.rm = TRUE), "extreme-temp records,",
        sum(extreme_socs, na.rm = TRUE), "extreme-soc records,",
        sum(missings, na.rm = TRUE), "incomplete records,",
        sum(singletons, na.rm = TRUE), "singletons, and",
        sum(wonky, na.rm = TRUE), "wonky lines of data\n")
  }
  ld <- ld |> filter_out(high_amps |
                           extreme_temps |
                           extreme_socs |
                           missings | singletons | wonky)
  if (trace > 0) {
    cat(" total remaining records:", nrow(ld), "\n")
  }
  stopifnot(nrow(ld) > 0)

  min_Hx <- round(min(ld$hx), 0)
  max_Hx <- round(max(ld$hx), 0)

  if (trace > 0) {
    cat(
      paste0(
        om$name,
        ": Hx = (",
        min_Hx,
        ", ",
        max_Hx,
        ")\n"
      )
    )
  }

  #enforce monotonicity in ocv table, using a least-squares fit
  ir <- isoreg(x = ld$soc, y = ld$ocv_estimate)
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
    sod = 1 - soc # "state of discharge"
  ) |>
    arrange(sod)
  ir2 <- isoreg(x = ld$sod, y = - ld$ocv_estimate)
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

  om$ocv_tbl2 <- newt2

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
    jsoc = jitter(soc, 0.0001)) # jitter to avoid ties
  if (methodology == "loess-sym") {
    # this may produce a better fit if there are outliers
    otf4 <- loess(ocv_estimate ~ jsoc, ld, family = "symmetric")
  } else {
    otf4 <- loess(ocv_estimate ~ jsoc, ld)
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
    dr <- cir::doseResponse(x = ld$jsoc, y = ld$ocv_estimate)
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
    e <-
      ggplot() +
      geom_point(data = ld,
                 aes(x = soc, y = pack_volts, colour = abs(pack_amps))) +
      geom_line(data = newt, aes(x = SOC, y = OCV), colour = "orange") +
      geom_line(data = newt2, aes(x = SOC, y = OCV), colour = "red") +
      geom_line(data = newt3, aes(x = SOC, y = OCV), colour = "green") +
      geom_line(data = newt4, aes(x = SOC, y = OCV), colour = "blue")
    if (methodology == "cir") {
      e <- e +
        geom_line(data = newt5, aes(x = SOC, y = OCV), colour = "violet")
    }
    plot(e +
      theme(palette.colour.continuous = "Okabe-Ito") +
      labs(
        title = paste0(
          om$name,
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

  om$ocv_tbl <- if (methodology == "isoreg") newt3 else
    if (methodology == "cir") newt5 else newt4

  # retval: an ocv_model with recomputed voltage predictions
  predict_volts(om = om, trace = trace)

}
