#' plot_volts_pred_soc: plot of prediction error in pack_volts by a
#' corrected soc
#'
#' @param m a thmodel or an ocv_model
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in ocv_model, ignored if !is.null(from_date)
#' @param to_idx ending index in ocv_model, ignored if !is.null(to_date)
#' @param from_temp lower limit of battery temps to be analysed
#' @param to_temp upper limit of battery temps to be analysed
#' @param max_amps limiting amperage for plotted points
#' @param wonky_threshold in Volts, outlier criterion (default 50)
#' @param scatter TRUE for scatterplot, FALSE for box and whiskers
#' @param nboxes controls the SOC precision of boxplots
#' @param by_amps colours indicate pack_temp (F), pack_amps (T)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_volts_pred_soc(predict_volts_tafel(eNV50kWh))
plot_volts_pred_soc <- function(m,
                            from_date = NULL,
                            to_date = NULL,
                            from_idx = NULL,
                            to_idx = NULL,
                            from_temp = NULL,
                            to_temp = NULL,
                            max_amps = NULL,
                            wonky_threshold = 50,
                            nboxes = 20,
                            scatter = FALSE,
                            by_amps = TRUE)
{
  pd <- select(m$logdata,
               date_time,
               pred_pack_volts_from_soc,
               pack_volts,
               pack_amps,
               soc,
               hx,
               pack_avg_temp
               )
  if (class(m) == "thmodel") {
    pd <- mutate(pd, soc = soc / 1e6)
  }

  # curiously, xts insists on UTC for stored dates & times
  from_idx <- ifelse(is.null(from_date),
                     ifelse(is.null(from_idx), 1, from_idx),
                     dplyr::first(which(
                       pd$date_time >= as.POSIXct(from_date, tz = "UTC")
                     )))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                   dplyr::last(which(
                     pd$date_time <= as.POSIXct(to_date, tz = "UTC")
                   )))
  if (is.na(from_idx) || is.na(to_idx)) {
    warning("Date out of range")
  } else if (from_idx >= to_idx) {
    warning("from_date is not before to_date")
  }

  pd <- pd |>
    slice(from_idx:to_idx) |>
    mutate(pred_error_pack_volts = pred_pack_volts_from_soc - pack_volts,
           delta_t = date_time - dplyr::lag(date_time))

  wonky <- (pd$pred_error_pack_volts > wonky_threshold)
  starts <- pd$delta_t > 120
  ends <- lead(starts)
  singletons <- starts & ends # unreliable readings
  missings <- is.na(pd$pack_volts) |
    is.na(pd$soc) |
    is.na(pd$pack_avg_temp) |
    is.na(pd$pack_amps) |
    is.na(pd$pred_pack_volts_from_soc)
  min_temp <- if (is.null(from_temp)) -30 else from_temp
  max_temp <- if (is.null(to_temp)) 100 else to_temp
  extreme_temps <- (pd$pack_avg_temp < min_temp) | (pd$pack_avg_temp > max_temp)
  high_amps <- if (is.null(max_amps))
    rep(F, length(pd$pack_amps)) else
      (abs(pd$pack_amps) > max_amps)
  cat("Filtering out",
      sum(high_amps, na.rm = TRUE), "high-amp records,",
      sum(extreme_temps, na.rm = TRUE), "extreme-temp records,",
      sum(missings, na.rm = TRUE), "incomplete records,",
      sum(singletons, na.rm = TRUE), "singletons, and",
      sum(wonky, na.rm = TRUE), "wonky lines of data\n")
  pd <- pd |>
    filter_out(singletons | wonky | missings | extreme_temps | high_amps)

  stopifnot(nrow(pd) > 0)

  min_Hx <- round(min(pd$hx), 0)
  max_Hx <- round(max(pd$hx), 0)
  temp_levels <- c(0, 10, 20, 30, 40)

  pd <- pd |> mutate(
    amperage = as_factor(round(pack_amps / 30, 0) * 30),
    temps = as_factor(round(pack_avg_temp / 10, 0) * 10),
    'SOC' = as_factor(round(nboxes*soc,0)/nboxes))
  suppressWarnings(
    pd <- pd |> mutate(
      temps = fct_recode(
        temps,
        "< 5\u2009°C" = "0",
        "[5, 15)\u2009°C" = "10",
        "[15, 25)\u2009°C" = "20",
        "[25, 35)\u2009°C" = "30",
        "> 35\u2009°C" = "40")))
  mycolors = c("< 5\u2009°C"  = "violet",
               "[5, 15)\u2009°C" = "blue",
               "[15, 25)\u2009°C" = "green",
               "[25, 35)\u2009°C" = "orange",
               "> 35\u2009°C" = "red")
  if (scatter) {
    e <- ggplot(pd, aes(x=SOC, y=pred_pack_volts_from_soc - pack_volts)) +
      theme(palette.colour.continuous = "Okabe-Ito")
    if (by_amps) {
      e <- e + geom_point(aes(colour = pack_amps))
    } else {
      e <- e + geom_point(aes(colour = pack_avg_temp))
    }
  } else {
    e <- ggplot(pd, aes(SOC, y=pred_pack_volts_from_soc - pack_volts))
    if (by_amps) {
      e <- e + geom_boxplot(aes(colour = amperage))
    } else {
      e <- e + geom_boxplot(aes(colour = temps)) +
        scale_color_manual(values=mycolors)
    }
  }

  e + labs(
    title =
      paste0(
        m$name,
        ": from #",
        from_idx,
        " to #",
        to_idx,
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
        ifelse(is.null(max_amps),
               "",
               paste0(", amps ≤ ", max_amps)),
        ". pi = ", round(m$parameters$polarisation_irr, 3), "\u2009mΩ, ",
        "ts = ", round(m$parameters$tafel_slope, 3), ", ",
        "λp = ", round(m$parameters$lambda_polarisation, 1), "\u2009s, ",
        "at = ", round(m$parameters$arrhenius_tafel, 0), ", ",
        "so = ", m$parameters$soc_offset_corr, ", ",
        "ss = ", m$parameters$soc_slope_corr, ", ",
        "Hx = (",
        min_Hx,
        ", ",
        max_Hx,
        ")"
      ),
    x = "LeafSpy SOC"
  )

}
