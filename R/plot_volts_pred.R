#' plot_volts_pred: plot of soc by prediction error in pack_volts
#'
#' @param m a thmodel or an ocv_model
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in ocv_model, ignored if !is.null(from_date)
#' @param to_idx ending index in ocv_model, ignored if !is.null(to_date)
#' @param wonky_threshold in Volts, outlier criterion (default 50)
#' @param scatter TRUE for scatterplot, FALSE for box and whiskers
#' @param scale_colours colours indicate pack_temp (0), pack_amps (1)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_volts_pred(om_eNV50kWh)
plot_volts_pred <- function(m,
                            from_date = NULL,
                            to_date = NULL,
                            from_idx = NULL,
                            to_idx = NULL,
                            wonky_threshold = 50,
                            scatter = FALSE,
                            scale_colours = 0)
{
  pd <- select(m$logdata,
               date_time,
               pred_pack_volts,
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
    mutate(pred_error_pack_volts = pred_pack_volts - pack_volts,
           delta_t = date_time - dplyr::lag(date_time))

  wonky <- (pd$pred_error_pack_volts > wonky_threshold)
  starts <- pd$delta_t > 120
  ends <- lead(starts)
  missings <- is.na(pd$pack_volts) |
    is.na(pd$soc) |
    is.na(pd$pack_avg_temp) |
    is.na(pd$pack_amps) |
    is.na(pd$pred_pack_volts)
  singletons <- starts & ends # unreliable readings
  cat("Filtering out",
      sum(missings, na.rm = TRUE), "incomplete records,",
      sum(singletons, na.rm = TRUE), "singletons, and",
      sum(wonky, na.rm = TRUE), "wonky lines of data\n")
  pd <- pd |> filter_out(singletons | wonky | missings)

  if (nrow(pd) == 0) {
    warning("No data to plot!")
  }

  min_Hx <- round(min(pd$hx), 0)
  max_Hx <- round(max(pd$hx), 0)

  pd <- pd |> mutate(
    amperage = as_factor(round(pack_amps/30, 0) * 30),
    temps = as_factor(if_else(
      pack_avg_temp < 20, "< 20",
      if_else(pack_avg_temp < 30, "[20, 30)",
              if_else(pack_avg_temp < 40, "[30, 40)",
                      ">= 40")))),
    'SOC' = as_factor(round(soc,1)))
  mycolors = c("< 20" = "blue",
               "[20, 30)"= "green",
               "[30, 40)" = "orange",
               ">= 40" = "red")

  if (scatter) {
    e <- ggplot(pd, aes(x=SOC, y=pred_pack_volts - pack_volts)) +
      theme(palette.colour.continuous = "Okabe-Ito")
    if (scale_colours == 0) {
      e <- e + geom_point(aes(colour = pack_avg_temp))
    } else if (scale_colours == 1) {
      e <- e + geom_point(aes(colour = pack_amps))
    }
  } else {
    e <- ggplot(pd, aes(SOC, y=pred_pack_volts - pack_volts))
    if (scale_colours == 0) {
      e <- e + geom_boxplot(aes(colour = temps)) +
        scale_color_manual(values=mycolors)
    } else if (scale_colours == 1) {
      e <- e + geom_boxplot(aes(colour = amperage))
    }
  }

  e + labs(title =
                  paste0(m$name,
                         ": from #", from_idx,
                         " to #", to_idx,
                         ". packr = ",
                         round(m$parameters[["effective_pack_resistance"]], 1),
                         "\u2009mΩ, r85 = ",
                         round(m$parameters[["packr85"]], 1),
                         "\u2009mΩ, arr = ",
                         round(m$parameters[["arrhenius_resistance"]], 0),
                         ", Hx = (", min_Hx, ", ", max_Hx, ")"
                  ))

}
