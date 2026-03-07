#' plot_log: plots distance, elevation, speed, SOC; geolocates in title
#'
#' @param m a thmodel
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_log(predict_temp())
plot_log <- function(m)
{
  #n.b. odo_km == 0 when the vehicle is in Acc mode
  pd <- m$logdata |>
    mutate(distance = ifelse(odo_km == 0, NA, odo_km - first(odo_km)),
           speed = smooth(speed),
           'elv/10' = smooth(elv) / 10,
           SOC = soc / 10000
    ) |>
    select(date_time,
           distance,
           speed,
           'elv/10',
           SOC
    ) |>
    as.xts()

  #n.b. GPS signals are not always available
  firstloc <- first(which(!is.na(m$logdata$lat)))
  lastloc <-  last(which(!is.na(m$logdata$lat)))
  startlat  <- substring(m$logdata$lat[firstloc], 1, 9)
  startlong <- substring(m$logdata$long[firstloc], 1, 9)
  lastlat   <- substring(m$logdata$lat[lastloc], 1, 9)
  lastlong  <- substring(m$logdata$long[lastloc], 1, 9)
  startloc <- paste0(startlat, ", ", startlong)
  endloc <- paste0(lastlat, ", ", lastlong)

  pd[,1:4] |>
    plot(
      legend.loc = "top",
      main.timespan = FALSE,
      main = paste0(
        m$name,
        ": from ",
        startloc,
        "; to ",
        endloc
      )
    )
}
