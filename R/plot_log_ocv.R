#' plot_log_ocv: plots pack_volts-est_ocv, pack_amps, soc
#'
#' @param m a thmodel
#' @param from_date starting date/time
#' @param to_date ending date/time
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_log_ocv(predict_temp())
plot_log_ocv <- function(m,
                     from_date = NULL,
                     to_date = NULL)
{
  # all logs "should" be sorted on date-time... but just in case...
  plotdata <- m$logdata |>
    arrange(date_time)
  # curiously, xts insists on UTC for stored dates & times
  from_date <- ifelse(is.null(from_date),
                      dplyr::first(plotdata$date_time),
                      from_date)
  to_date <- ifelse(is.null(to_date),
                    dplyr::last(plotdata$date_time),
                    to_date)
  from_idx <- dplyr::first(
    which(plotdata$date_time >= as.POSIXct(from_date, tz = "UTC")))
  to_idx <- dplyr::last(
    which(plotdata$date_time <= as.POSIXct(to_date, tz = "UTC")))
  stopifnot(!is.na(from_idx) && !is.na(to_idx) && (from_idx < to_idx))

  x <- plotdata |>
    slice(from_idx:to_idx) |>
    mutate(SOC = soc / 10000,
           temp = pack_avg_temp,
           pred_temp = pred_pack_avg_temp,
           `pack_volts - 300` = pack_volts - 300,
           `est_overvoltage * 10` = 10 * (pack_volts - est_ocv)
           ) |>
    select(date_time, `est_overvoltage * 10`,
           `pack_volts - 300`,
           pack_amps, SOC, temp, pred_temp) |>
    xts::as.xts()

  plot(
    x,
    legend.loc = "top",
    format.labels = "%Y-%m-%d %H:%M",
    main.timespan = FALSE,
    main = paste0(
      m$name,
      "(", from_date, ", ", to_date,
      "): r = ",
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
