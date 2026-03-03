# Plotting utilities

library(tidyverse)
library(xts)



#' Plot pack temp, predicted pack temp, and ambient temp
#'
#' #todo: dispatch through generic plot()
#'
#' @param m
#'
#' @returns
#' @export
#'
#' @examples
#' plot_fit(predict_temp())
#' plot_fit(fit_model())
plot_fit <- function(m)
{
  m$logdata |>
    select(date_time, pack_avg_temp, pred_pack_avg_temp, ambient) |>
    as.xts() |>
    plot(legend.loc = "right",
         main.timespan = FALSE,
         main = paste0(m$name,
                      ": r = ",
                      format((m$parameters)[[1]], digits = 3),
                      " Ω, λ_1 = ",
                      format((m$parameters)[[2]], digits = 3),
                      " s, λ_2 = ",
                      format((m$parameters)[[3]]/3600, digits = 3),
                      " h")
    )
}
