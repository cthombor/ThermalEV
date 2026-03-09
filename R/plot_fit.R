#' Plot pack temp, predicted pack temp, ambient temp, kW (smoothed)
#'
#' #todo: dispatch through generic plot()
#'
#' @param m
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_fit(predict_temp())
#' plot_fit(fit_model())
plot_fit <- function(m)
{
  pd <- m$logdata |>
    mutate(charging_kW =
             smooth(
               ifelse(pack_amps > 0, 0, -pack_amps * pack_volts)) / 1000,
           discharge_kW =
             smooth(
               ifelse(pack_amps > 0, pack_amps * pack_volts, 0)) / 1000
           ) |>
    select(date_time,
           pack_avg_temp,
           pred_pack_avg_temp,
           ambient,
           charging_kW,
           discharge_kW) |>
    as.xts()
  pd |>
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
