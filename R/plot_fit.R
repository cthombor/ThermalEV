#' Plot pack temp, predicted pack temp, ambient temp, kW (smoothed)
#'
#' #todo: dispatch through generic plot()
#'
#' @param m a thmodel, with predictions
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_fit(predict_temp())
#' plot_fit(predict_temp(), to_idx = 10)
#' plot_fit(predict_temp(), to_date = "2026-01-26 11:00")
#' plot_fit(fit_model())
plot_fit <- function(m,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL
)
{
  plotdata <- m$logdata |> arrange(date_time)
  # curiously, xts insists on UTC for stored dates & times
  from_idx <- ifelse(is.null(from_date),
                     ifelse(is.null(from_idx), 1, from_idx),
                     dplyr::first(which(
                       plotdata$date_time >= as.POSIXct(from_date, tz = "UTC")
                     )))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                   dplyr::last(which(
                     plotdata$date_time <= as.POSIXct(to_date, tz = "UTC")
                   )))
  if (is.null(from_idx) || is.null(to_idx) || from_idx > to_idx) {
    warning("No data to plot!")
  }
  pd <- plotdata |>
    slice(from_idx:to_idx) |>
    mutate(charging_kW =
             smooth(
               ifelse(pack_amps > 0, 0, -pack_amps * pack_volts)) / 1000,
           discharge_kW =
             smooth(
               ifelse(pack_amps > 0, pack_amps * pack_volts, 0)) / 1000,
           `AC power/10` = est_pwr_a_c_50w * 5
           ) |>
    select(date_time,
           pack_avg_temp,
           pred_pack_avg_temp,
           ambient,
           charging_kW,
           discharge_kW,
           `AC power/10`) |>
    as.xts()
  pd |>
    plot(
      legend.loc = "top",
      type = "p",
      pch = 1,
      main.timespan = FALSE,
      format.labels = "%y-%m-%d %H:%M",
      main = paste0(
        m$name,
        ": r = ",
        format((m$parameters)[[1]], digits = 3),
        " mΩ, λ1 = ",
        format((m$parameters)[[2]], digits = 3),
        " s, λ2 = ",
        format((m$parameters)[[3]], digits = 3),
        " h"
      )
    )
}
