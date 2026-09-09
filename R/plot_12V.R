#' plot_12V: plots distance, speed, LVDC volts and amps; geolocates in title
#'
#' @param m a thmodel
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_12V(predict_temp())
#' plot_12V(predict_temp(), 1, 10)
plot_12V <- function(m,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL)
{
  # all logs "should" be sorted on date-time... but just in case...
  plotdata <- m$logdata |> arrange(date_time)
  # curiously, xts insists on UTC for stored dates & times
  from_idx <- ifelse(is.null(from_date),
                     ifelse(is.null(from_idx), 1, from_idx),
                     dplyr::first(which(
                       plotdata$date_time >= as.POSIXct(from_date, tz = "UTC")
                     )))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                   dplyr::last(which(
                     plotdata$date_time <= as.POSIXct(to_date, tz = "UTC")
                   )))
  if (is.na(from_idx) || is.na(to_idx)) {
    warning("Date out of range")
  } else if (from_idx >= to_idx) {
    warning("from_date is not before to_date")
  }

  plotdata <- plotdata |> slice(from_idx:to_idx)

  #n.b. In the unmunged LeafSpy csv logs, odo_km == 0 when the vehicle is not
  #in Drive. In the munged logs, odo_km == NA when the vehicle is not in Drive.
  firstodo <- plotdata$odo_km[
    dplyr::first(which(!is.na(plotdata$odo_km)))]

  #n.b. I plot data from what LeafSpy describes as speed sensor 1 rather than
  #from the "sensor 2" or the GPS-reported speed. The GPS reports are
  #incomplete, and I'd expect them to be inaccurate on curvy roads.  Empirical
  #evidence strongly suggests to me that Speed2 is the (significantly
  #over-reported) value reported on the dashboard, and that the smaller Speed1
  #value is a best-effort estimation of speed (which I presume would be unbiased
  #if the vehicle has stock wheels and newly-fitted tyres).
  x <- plotdata |>
    mutate(distance = odo_km - firstodo,
           'distance/10' = distance / 10,
           'distance/100' = distance / 100,
           speed1s = smooth(speed1) / 100,
           'LVDC_V*10' = 10 * as.numeric(x12v_bat_volts),
           'LVDC_A*10' = 10 * as.numeric(x12v_bat_amps)
    )

  if (max(x$distance, na.rm = TRUE) < 150) {
    x <- x |>
      select(date_time,
             distance,
             speed1s,
             'LVDC_V*10',
             'LVDC_A*10'
      ) |>
      as.xts()
  } else if (max(x$distance, na.rm = TRUE) < 1500) {
    x <- x |>
      select(date_time,
             'distance/10',
             speed1s,
             'LVDC_V*10',
             'LVDC_A*10'
      ) |>
             as.xts()
  } else {
    x <- x |>
      select(date_time,
             'distance/100',
             speed1s,
             'LVDC_V*10',
             'LVDC_A*10'
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
    format.labels = "%Y-%m-%d %H:%M",
    main = paste0(m$name, ": from (", startloc, ") to (", endloc, ")")
  )
}
