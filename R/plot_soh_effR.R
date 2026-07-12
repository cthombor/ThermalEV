#' plot_soh_effR: plots SOH, Hx, pack temps, and estimated effR
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
#' plot_soh_effR(predict_temp())
#' plot_soh_effR(predict_temp(), from_idx=1, to_idx=10)
plot_soh_effR <- function(m,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL)
{
  plotdata <- m$logdata |> arrange(date_time)
  arrhenius_coeff <- m$parameters[["arrhenius_resistance"]]
  effective_pack_resistance <- m$parameters[["effective_pack_resistance"]]
  # curiously, xts insists on UTC for stored dates & times
  if (!is.null(from_date)) {
    from_date <- as.POSIXct(from_date, tz = "UTC")
  }
  if (!is.null(to_date)) {
    to_date <- as.POSIXct(to_date, tz = "UTC")
  }
  from_idx <- ifelse(is.null(from_date),
                     ifelse(is.null(from_idx), 1, from_idx),
                     dplyr::first(which(plotdata$date_time >= from_date)))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                   dplyr::last(which(plotdata$date_time <= to_date)))
  if (is.na(from_idx) || is.na(to_idx)) {
    warning("Date out of range")
  } else if (to_idx < from_idx) {
    warning("No data in this range")
  } else {
    plotdata <- plotdata |>
      slice(from_idx:to_idx)

    #n.b. In the unmunged LeafSpy csv logs, odo_km == 0 when the vehicle is not
    #in Drive. In the munged logs, odo_km == NA when the vehicle is not in Drive.
    firstodo <- plotdata$odo_km[
      dplyr::first(which(!is.na(plotdata$odo_km)))]

    x <- plotdata |>
      mutate(SOH = soh,
             Hx = hx,
             'eff_packr/10' = effective_pack_resistance *
               exp(arrhenius_coeff *
                     (1 / 298.15 - 1 / (pack_avg_temp + 273.15))) /
               (hx / 100) / 10
      ) |>
      select(date_time,
             SOH,
             Hx,
             'eff_packr/10',
             pack_avg_temp
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

    plot(
      x,
      legend.loc = "top",
      main.timespan = FALSE,
      format.labels = "%Y-%m-%d %H:%M",
      main = paste0(m$name, ": ",
                    effective_pack_resistance, " mΩ, arr = ",
                    arrhenius_coeff)
    )
  }
}
