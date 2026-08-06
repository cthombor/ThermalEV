#' plot_volts: scatterplot of pack_volts v scaled gids
#'
#' @param m a thmodel with predictions (requires m$pack_avg_temp)
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#' @param min_sgids high-pass filter on scaled gids (to examine non-linearity)
#' @param max_sgids low-pass filter on scaled gids (to examine non-linearity)
#' @param scale_colours colours show pack_temp (0), pack_amps (1), pack_kW (2)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_volts(eNV200ac24kWh_2025)
#' plot_volts(eNV200ac24kWh_2025, max_sgids = 75)
#' plot_volts(eNV200ac24kWh_2025, max_sgids = 75, temp_colours=FALSE)
#' plot_volts(eNV200ac24kWh_2025, max_sgids = 75, temp_colours=TRUE)
plot_volts <- function(m,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL,
                     min_sgids = NULL,
                     max_sgids = NULL,
                     scale_colours = 0)
{
  pd <- m$logdata |> arrange(date_time)
  # curiously, xts insists on UTC for stored dates & times
  if (!is.null(from_date)) {
    from_date <- as.POSIXct(from_date, tz = "UTC")
  }
  if (!is.null(to_date)) {
    to_date <- as.POSIXct(to_date, tz = "UTC")
  }
  from_idx <- ifelse(is.null(from_date),
                     ifelse(is.null(from_idx), 1, from_idx),
                     dplyr::first(which(pd$date_time >= from_date)))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                   dplyr::last(which(pd$date_time <= to_date)))
  if (is.na(from_idx) || is.na(to_idx)) {
    warning("Date out of range")
  } else if (to_idx < from_idx) {
    warning("No data in this range")
  } else {
    pd <- pd |>
      slice(from_idx:to_idx)
    min_Hx <- round(min(pd$hx), 0)
    max_Hx <- round(max(pd$hx), 0)
    min_SOH <- round(min(pd$soh), 0)
    max_SOH <- round(max(pd$soh), 0)
    pd <- pd |>
#      select(date_time, gids, soc, soh, pack_volts, pack_avg_temp,
#             pack_amps) |>
      mutate(gids_scaled = gids / (soh / 100),
             soc = soc / 1e4,
             pack_volts = ifelse(pack_volts == 0, NA, pack_volts),
             pack_kW = pack_amps * pack_volts / 1000,
             gids_scaled = ifelse(gids_scaled == 0, NA, gids_scaled),
             gids_ratio = gids_scaled / soc)
    if (!is.null(max_sgids))
      pd <- pd[(pd$gids_scaled <= max_sgids), ]
    if (!is.null(min_sgids))
      pd <- pd[(pd$gids_scaled >= min_sgids), ]

    if (nrow(pd) == 0) {
      warning("No data to plot!")
    }

    if (scale_colours == 0) {
      e <- ggplot(pd, aes(x=gids_scaled, y=pack_volts)) +
        theme(palette.colour.continuous = "Okabe-Ito") +
        geom_point(aes(colour = pack_avg_temp))
    } else if (scale_colours == 1) {
      e <- ggplot(pd, aes(x=gids_scaled, y=pack_volts)) +
        theme(palette.colour.continuous = "Okabe-Ito") +
        geom_point(aes(colour = pack_amps))
    } else {
      e <- ggplot(pd, aes(x=gids_scaled, y=pack_volts)) +
        theme(palette.colour.continuous = "Okabe-Ito") +
        geom_point(aes(colour = pack_kW))
    }
    e + labs(title = paste0(m$name,
                            ": from #", from_idx,
                            " to #", to_idx,
                            ". Hx = (", min_Hx, ", ", max_Hx, ")",
                            ", SOH = (", min_SOH, ", ", max_SOH, ")"
                            ))
  }
}
