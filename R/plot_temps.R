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
  if (!is.null(from_date)) {
    from_date <- as.POSIXct(from_date, tz = "UTC")
  }
  if (!is.null(to_date)) {
    to_date <- as.POSIXct(to_date, tz = "UTC")
  }
  from_idx <- ifelse(is.null(from_date),
                     ifelse(is.null(from_idx), 1, from_idx),
                     dplyr::first(which(plotdata$date_time >= from_date)))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                   dplyr::last(which(plotdata$date_time <= to_date)))
  if (is.null(from_idx) || is.null(to_idx)) {
    warning("Date out of range")
  } else if (from_idx > to_idx) {
    warning("from_date is after to_date")
  }

  plotdata <- plotdata |> slice(from_idx:to_idx)

  x <- plotdata |>
    select(date_time, pack_t1_c, pack_t2_c, pack_t4_c, pack_avg_temp,
           pred_pack_avg_temp)

  x |>
    xts::as.xts() |>
    plot(
      legend.loc = "top",
      type = "p",
      pch = 1,
      main.timespan = FALSE,
      format.labels = "%Y-%m-%d %H:%M",
      main = paste0(
        m$name,
        ": r = ",
        format((m$parameters)[["effective_pack_resistance"]], digits = 3),
        # n.b. U+2009 is a thin space
        ifelse((m$parameters)[["packr85"]] !=
                 (m$parameters)[["effective_pack_resistance"]],
               paste0("\u2009mΩ, packr85 = ",
                      format((m$parameters)[["packr85"]], digits = 3),
                      collapse = ""),
               ""
        ),
        "\u2009mΩ, pr = ",
        format((m$parameters)[["polarisation_rev"]], digits = 3),
        "\u2009kJ/V, pi = ",
        format((m$parameters)[["polarisation_irr"]], digits = 3),
        ", λp = ",
        format((m$parameters)[["lambda_module_to_ambient"]], digits = 3),
        "\u2009h, λa = ",
        format((m$parameters)[["lambda_module_AC_to_ambient"]], digits = 3),
        "\u2009h, fanp = ",
        format((m$parameters)[["fan_power"]], digits = 3),
        "\u2009W,\n     COP = ",
        format((m$parameters)[["COP"]], digits = 3),
        ", a = ",
        format((m$parameters)[["arrhenius_resistance"]], digits = 3),
        ", c = ",
        format((m$parameters)[["heat_capacity"]], digits = 3),
        "\u2009kJ/K"
      )
    )
}
