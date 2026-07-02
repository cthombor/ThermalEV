#' plot_soh: plots SOH and Hx; geolocates in title
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
#' plot_soh(predict_temp())
#' plot_soh(predict_temp(), from_idx=1, to_idx=10)
plot_soh <- function(m,
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
  if (is.null(from_idx) || is.null(to_idx)) {
    warning("Date out of range")
  } else if (from_idx >= to_idx) {
    warning("from_date is not before to_date")
  }

  plotdata <- plotdata |> slice(from_idx:to_idx)

  #n.b. In the unmunged LeafSpy csv logs, odo_km == 0 when the vehicle is not
  #in Drive. In the munged logs, odo_km == NA when the vehicle is not in Drive.
  firstodo <- plotdata$odo_km[
    dplyr::first(which(!is.na(plotdata$odo_km)))]

  x <- plotdata |>
    mutate(SOH = soh,
           Hx = hx
    ) |>
    select(date_time,
           SOH,
           Hx
    )

  maxsoh <- which.max(x$SOH)
  minsoh <- which.min(x$SOH)
  cat("Min SOH",
      round(x$SOH[minsoh], 2),
      "at",
      format_ISO8601(x$date_time[minsoh]),
      "\n")
  cat("Max SOH",
      round(x$SOH[maxsoh], 2),
      "at",
      format_ISO8601(x$date_time[maxsoh]),
      "\n")

  maxhx <- which.max(x$Hx)
  minhx <- which.min(x$Hx)
  cat("Min Hx",
      round(x$Hx[minhx], 2),
      "at",
      format_ISO8601(x$date_time[minhx]),
      "\n")
  cat("Max Hx",
      round(x$Hx[maxhx], 2),
      "at",
      format_ISO8601(x$date_time[maxhx]),
      "\n")

  x <- x |>
      as.xts()

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
