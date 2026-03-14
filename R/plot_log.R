#' plot_log: plots distance, elevation, speed, SOC; geolocates in title
#'
#' @param m a thmodel
#' @param from_idx starting index in thmodel
#' @param to_idx ending index in thmodel
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_log(predict_temp())
#' plot_log(predict_temp(), 1, 10)
plot_log <- function(m, from_idx=NULL, to_idx=NULL)
{
  #n.b. odo_km == NA when the vehicle is not in Drive mode.  (In the
  #unmunged LeafSpy csv logs, odo_km == 0 when the vehicle is not
  #in Drive.)

  from_idx <- ifelse(is.null(from_idx), 1, from_idx)
  to_idx <- ifelse(is.null(to_idx), nrow(m$logdata), to_idx)
  plotdata <- m$logdata |> slice(from_idx:to_idx)

  firstodo <- plotdata$odo_km[
    dplyr::first(which(!is.na(plotdata$odo_km)))]

  x <- plotdata |>
    mutate(distance = odo_km - firstodo,
           'distance/10' = distance / 10,
           'distance/100' = distance / 100,
           speed = smooth(speed),
           'elv/10' = smooth(elv) / 10,
           SOC = soc / 10000
    )
  if (max(x$distance, na.rm = TRUE) < 150) {
    x <- x |>
      select(date_time,
             distance,
             speed,
             'elv/10',
             SOC
      ) |>
      as.xts()
  } else if (max(x$distance, na.rm = TRUE) < 1500) {
    x <- x |>
      select(date_time,
             'distance/10',
             speed,
             'elv/10',
             SOC
      ) |>
      as.xts()
  } else {
    x <- x |>
      select(date_time,
             'distance/100',
             speed,
             'elv/10',
             SOC
      ) |>
      as.xts()
  }

  #n.b. GPS signals are not always available
  firstloc <- dplyr::first(which(!is.na(plotdata$lat)))
  lastloc <-  dplyr::last(which(!is.na(plotdata$lat)))
  startlat  <- round(parzer::parse_lat(plotdata$lat[firstloc]),2)
  startlong <- round(parzer::parse_lon(plotdata$long[firstloc]),2)
  lastlat   <- round(parzer::parse_lat(plotdata$lat[lastloc]),2)
  lastlong  <- round(parzer::parse_lon(plotdata$long[lastloc]),2)
  startloc <- paste0(startlat, ", ", startlong)
  endloc <- paste0(lastlat, ", ", lastlong)

  plot(
    x,
    legend.loc = "top",
    main.timespan = FALSE,
    main = paste0(m$name, ": from (", startloc, ") to (", endloc, ")")
  )
}
