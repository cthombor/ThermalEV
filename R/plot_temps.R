#' plot_temps: plots temperatures of all sensors, mean temp, and predicted temp
#' @param m a thmodel
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_temps(predict_temp())
plot_temps <- function(m)
{
  x <- m$logdata |>
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
