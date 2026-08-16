#' plot_volts_pred: plot of soc by prediction error in pack_volts
#'
#' @param om an ocv_model
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in ocv_model, ignored if !is.null(from_date)
#' @param to_idx ending index in ocv_model, ignored if !is.null(to_date)
#' @param scatter TRUE for scatterplot, FALSE for box and whiskers
#' @param scale_colours colours indicate pack_temp (0), pack_amps (1)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_volts_pred(predict_ocv(new_ocv_model("eNV50kWh", list(eNV200ac50kWh)), 67, -3500))
plot_volts_pred <- function(om,
                            from_date = NULL,
                            to_date = NULL,
                            from_idx = NULL,
                            to_idx = NULL,
                            scatter = FALSE,
                            scale_colours = 0)
{
  pd <- om$logdata

  # curiously, xts insists on UTC for stored dates & times
  from_idx <- ifelse(is.null(from_date),
                     ifelse(is.null(from_idx), 1, from_idx),
                     dplyr::first(which(
                       pd$date_time >= as.POSIXct(from_date, tz = "UTC")
                     )))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(om$logdata), to_idx),
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
    mutate(pred_error_pack_volts = pred_pack_volts - pack_volts)

  if (nrow(pd) == 0) {
    warning("No data to plot!")
  }

  min_Hx <- round(min(pd$hx), 0)
  max_Hx <- round(max(pd$hx), 0)

  pd <- pd |> mutate(
    charging = as_factor(if_else(
      pack_amps > 0, "discharge", "charge")),
    temps = as_factor(if_else(
      pack_avg_temp < 20, "< 20",
      if_else(pack_avg_temp < 30, "[20, 30)",
              if_else(pack_avg_temp < 40, "[30, 40)",
                      ">= 40")))),
    socs = as_factor(round(soc,1)))
  mycolors = c("< 20" = "blue",
               "[20, 30)"= "green",
               "[30, 40)" = "orange",
               ">= 40" = "red")

  if (scatter) {
    e <- ggplot(pd, aes(x=soc, y=pred_error_pack_volts)) +
      theme(palette.colour.continuous = "Okabe-Ito")
    if (scale_colours == 0) {
      e <- e + geom_point(aes(colour = pack_avg_temp))
    } else if (scale_colours == 1) {
      e <- e + geom_point(aes(colour = pack_amps))
    }
  } else {
    e <- ggplot(pd, aes(socs, pred_error_pack_volts))
    if (scale_colours == 0) {
      e <- e + geom_boxplot(aes(colour = temps)) +
        scale_color_manual(values=mycolors)
    } else if (scale_colours == 1) {
      e <- e + geom_boxplot(aes(colour = charging))
    }
  }

  e + labs(title =
                  paste0(om$name,
                         ": from #", from_idx,
                         " to #", to_idx,
                         ". packr = ",
                         round(om$parameters[["effective_pack_resistance"]], 1),
                         ", r85 = ",
                         round(om$parameters[["packr85"]], 1),
                         ", arr = ",
                         round(om$parameters[["arrhenius_resistance"]], 0),
                         ", Hx = (", min_Hx, ", ", max_Hx, ")"
                  ))

}
