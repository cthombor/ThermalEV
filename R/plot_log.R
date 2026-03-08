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
  #n.b. odo_km == NA when the vehicle is not in Drive mode.  (In the
  #unmunged LeafSpy csv logs, odo_km == 0 when the vehicle is not
  #in Drive.)

  firstodo <- m$logdata$odo_km[first(which(!is.na(m$logdata$odo_km)))]

  pd <- m$logdata |>
    mutate(distance = odo_km - firstodo,
           'distance/10' = distance / 10,
           speed = smooth(speed),
           'elv/10' = smooth(elv) / 10,
           SOC = soc / 10000
    )
  if (max(pd$distance, na.rm = TRUE) < 150) {
    pd <- pd |>
      select(date_time,
             distance,
             speed,
             'elv/10',
             SOC
      ) |>
      as.xts()
  } else {
    pd <- pd |>
      select(date_time,
             'distance/10',
             speed,
             'elv/10',
             SOC
      ) |>
      as.xts()
  }

  #n.b. GPS signals are not always available
  firstloc <- first(which(!is.na(m$logdata$lat)))
  lastloc <-  last(which(!is.na(m$logdata$lat)))
  startlat  <- round(parzer::parse_lat(m$logdata$lat[firstloc]),2)
  startlong <- round(parzer::parse_lon(m$logdata$long[firstloc]),2)
  lastlat   <- round(parzer::parse_lat(m$logdata$lat[lastloc]),2)
  lastlong  <- round(parzer::parse_lon(m$logdata$long[lastloc]),2)
  startloc <- paste0(startlat, ", ", startlong)
  endloc <- paste0(lastlat, ", ", lastlong)

  pd |>
    plot(
      legend.loc = "top",
      main.timespan = FALSE,
      main = paste0(
        m$name,
        ": from (",
        startloc,
        ") to (",
        endloc,
        ")"
      )
    )
}
