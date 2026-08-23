#' Plot pack temp, predicted pack temp, ambient temp, kW (smoothed)
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
#' plot_fit(eNV200noac50kWh, from_idx = 1, to_idx = 10)
#' plot_fit(eNV200ac50kWh, "2026-01-26 11:00", "2026-01-27")
plot_fit <- function(m,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL
)
{
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
  if (is.na(from_idx) || is.na(to_idx)) {
    warning("Date out of range")
  } else if (to_idx < from_idx) {
    warning("No data in this range")
  } else {
    pd <- plotdata |>
      slice(from_idx:to_idx) |>
      mutate(pack_volts = ifelse(is.na(pack_volts),
                                 median(dplyr::lag(pack_volts),
                                        dplyr::lead(pack_volts),
                                        na.rm=TRUE),
                                 pack_volts)
      ) |>
      mutate(
        charging_kW =
          smooth(ifelse(pack_amps > 0, 0, -pack_amps * pack_volts)) / 1000,
        discharge_kW =
          smooth(ifelse(pack_amps > 0, pack_amps * pack_volts, 0)) / 1000,
        `AC power/100` = est_pwr_a_c_50w * 0.5
        # `AC power/100` = a_c_pwr_250w * 2.5 # less precise, same mean as 50W
      )

    maxpe <- which.max(pd$err_pred)
    minpe <- which.min(pd$err_pred)
    cat("Underprediction by",
        round(pd$err_pred[minpe], 2),
        "degrees at",
        format_ISO8601(pd$date_time[minpe]),
        "\n")
    cat("Overprediction by",
        round(pd$err_pred[maxpe], 2),
        "degrees at",
        format_ISO8601(pd$date_time[maxpe]),
        "\n")

    # mod <- lm(err_pred ~ slope_amps + acc_amps, pd)
    # print(broom::tidy(mod))
    # print(broom::tidy(anova(mod)))

    pd <- pd |>
      select(date_time,
             pack_avg_temp,
             pred_pack_avg_temp,
             ambient,
             charging_kW,
             discharge_kW,
             `AC power/100`) |>
      as.xts()
    pd |>
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
          "\u2009mΩ, r85 = ", # n.b. unicode is a thin space
          format((m$parameters)[["packr85"]], digits = 3),
          "\u2009mΩ, p = ",
          format((m$parameters)[["polarisation_energy"]], digits = 3),
          "\u2009kJ/V, λp = ",
          format((m$parameters)[["lambda_module_to_ambient"]], digits = 3),
          "\u2009h, λa = ",
          format((m$parameters)[["lambda_module_AC_to_ambient"]], digits = 3),
          "\u2009h, fanp = ",
          format((m$parameters)[["fan_power"]], digits = 3),
          "\u2009W, COP = ",
          format((m$parameters)[["COP"]], digits = 3),
          ", a = ",
          format((m$parameters)[["arrhenius_resistance"]], digits = 3),
          ", c = ",
          format((m$parameters)[["heat_capacity"]], digits = 3),
          "\u2009kJ/K"
        )
      )
  }

}
