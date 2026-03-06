# Plotting utilities

library(tidyverse)
library(xts)

#' Plot pack temp, predicted pack temp, ambient temp, pack kW
#'
#' Not working yet -- pack kW should be on a separate panel, or smoothed
#' so that it's not so intrusive
#'
#' #todo: dispatch through generic plot()
#'
#' @param m
#'
#' @returns 1
#' @export
#'
#' @examples
#' plot_fit(predict_temp())
#' plot_fit(fit_model())
plot_fit <- function(m)
{
  browser()
  pd <- m$logdata |>
    mutate(
      smoothed_kW = smooth.spline(pack_kW)$y
    ) |>
    select(date_time,
           pack_avg_temp,
           pred_pack_avg_temp,
           ambient,
           smoothed_kW) |>
    as.xts()
  pd |>
    plot(
      legend.loc = "top",
      main.timespan = FALSE,
      #        multi.panel = 3,
      #        y.axis = same,
      main = paste0(
        m$name,
        ": r = ",
        format((m$parameters)[[1]] * 1000, digits = 3),
        " mΩ, λ1 = ",
        format((m$parameters)[[2]], digits = 3),
        " s, λ2 = ",
        format((m$parameters)[[3]] / 3600, digits = 3),
        " h"
      )
    )
}
