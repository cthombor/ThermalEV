#' plot_temps: plots temperatures of all sensors, mean temp, and predicted temp
#' @param m a thmodel with temperature predictions
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_temps(predict_temp())
#' plot_temps(eNV200ac24kWh_2024, from_date="2024-02-21", to_date="2024-02-22")
plot_temps <- function(
    m,
    from_date = NULL,
    to_date = NULL,
    from_idx = NULL,
    to_idx = NULL)
{
  # all logs "should" be sorted on date-time... but just in case...
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

  plotdata <- plotdata |> slice(from_idx:to_idx)

  x <- plotdata |>
    select(date_time, pack_t1_c, pack_t2_c, pack_t4_c, pack_avg_temp,
           pred_pack_avg_temp)

  x |>
    as.xts() |>
    plot(
      legend.loc = "top",
      type = "p",
      pch = 1,
      main.timespan = FALSE,
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
