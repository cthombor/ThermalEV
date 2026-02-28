# Plotting utilities

library(tidyverse)
library(xts)

plot_fit <- function()
{
  global_logtibble |>
    select(date_time, pack_avg_temp, pred_pack_avg_temp, ambient) |>
    as.xts() |>
    plot(legend.loc = "right",
         main.timespan = FALSE,
         main = paste0(global_filnm,
                      ".csv: r = ",
                      format(global_fit$estimate[1], digits = 3),
                      " Ω, λ_1 = ",
                      format(global_fit$estimate[2], digits = 3),
                      " s, λ_2 = ",
                      format(global_fit$estimate[3]/3600, digits = 3),
                      " h")
    )
}
