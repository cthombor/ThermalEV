#' est_ocv: refine an ocv_tbl, from errors in an om_model's voltage prediction
#'
#' Usage notes.  You might want to interpolate a smaller table e.g.
#'  ot <- est_ocv(om_eNV50kWh)
#'  otf <- approxfun(x = ot$SOC, y = ot$OCV, method = "linear", rule = 2)
#'  oti <- tibble(SOC=c(0:20)/rep(20,21)) |> mutate(OCV = otf(SOC)))
#'  e50 <- predict_temp(eNV200ac50kWh, ocv_tbl = oti)
#'
#' @param om an ocv_model
#' @param wonky_threshold in Volts, outlier criterion (default 50)
#' @param trace 0 for silent, 1 for minimal, 2 for verbose
#'
#' @returns an ocv_model with an updated ocv_tbl and voltage predictions
#' @export
#'
#' @examples
#' est_ocv(om_eNV50kWh)
est_ocv <- function(om,
                    wonky_threshold = 50,
                    trace = 2)
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
  if (trace > 0) {
    cat("est_ocv: filtering out",
        sum(missings, na.rm = TRUE), "incomplete records,",
        sum(singletons, na.rm = TRUE), "singletons, and",
        sum(wonky, na.rm = TRUE), "wonky lines of data\n")
  }
  ld <- ld |> filter_out(missings | singletons | wonky)

  if (nrow(ld) == 0) {
    warning("No data to analyse!")
  }

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
                 SOC < ir$x[[first(ir$iKnots)]],
                 OCV < ir$yf[[first(ir$iKnots)]])
  othi <- filter(ot,
                 SOC > ir$x[[last(ir$iKnots)]],
                 OCV > ir$yf[[last(ir$iKnots)]])
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

  om$ocv_tbl <- newt

  # retval: an ocv_model with recomputed voltage predictions
  predict_volts(om = om, trace = trace)

}
