#' plot_volts: scatterplot of pack_volts v scaled gids
#'
#' @param m a thmodel with predictions (requires m$pack_avg_temp)
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#' @param min_sgids high-pass filter on scaled gids (to examine non-linearity)
#' @param max_sgids low-pass filter on scaled gids (to examine non-linearity)
#' @param volts_per_cell: T to divide pack_volts by the number of cell(pair)s
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
                     volts_per_cell = T,
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
  if (is.na(from_idx) || is.na(to_idx))
    warning("Date out of range")
  stopifnot (from_idx <= to_idx)

  pd <- pd |>
    slice(from_idx:to_idx)

  if (!is.null(max_sgids))
    pd <- pd[(pd$gids_scaled <= max_sgids), ]
  if (!is.null(min_sgids))
    pd <- pd[(pd$gids_scaled >= min_sgids), ]

  missings <- is.na(pd$gids) |
    is.na(pd$soc) |
    is.na(pd$soh) |
    is.na(pd$pack_avg_temp) |
    is.na(pd$pack_amps) |
    is.na(pd$pack_volts) |
    is.na(pd$avg_cp_m_v) |
    (pd$pack_volts == 0)
# unreliable <- pd$segnum == 0
  wonky <- pd$pack_volts < 300

  cat("Filtering out",
#     sum(unreliable, na.rm = TRUE), "unreliable records,",
      sum(wonky, na.rm = TRUE), "wonky records, and",
      sum(missings, na.rm = TRUE), "incomplete records\n")
  pd <- pd |>
    filter_out(
#     unreliable |
      wonky |
      missings)
  stopifnot(nrow(pd) > 0)

  min_Hx <- round(min(pd$hx), 0)
  max_Hx <- round(max(pd$hx), 0)
  min_SOH <- round(min(pd$soh), 0)
  max_SOH <- round(max(pd$soh), 0)
  pd <- pd |>
    mutate(
      gids_scaled = gids / (soh / 100),
      soc = soc / 1e4,
      volts = if (volts_per_cell)
        pack_volts / 96
      else
        pack_volts,
      pack_kW = pack_amps * pack_volts / 1000,
      gids_scaled = ifelse(gids_scaled == 0, NA, gids_scaled),
      gids_ratio = gids_scaled / soc
    )

  cat("Mean Cell Voltage - Pack Voltage / 96:\n")
  print(summary(
    pd$avg_cp_m_v / 1000 - pd$pack_volts / 96))
  cat("Min cell voltage:", min(pd$min_cp_m_v / 1000), "\n")
  cat("Max cell voltage:", max(pd$max_cp_m_v / 1000), "\n")

  if (scale_colours == 0) {
    e <- ggplot(pd, aes(x = gids_scaled, y = volts)) +
      theme(palette.colour.continuous = "Okabe-Ito") +
      geom_point(aes(colour = pack_avg_temp))
  } else if (scale_colours == 1) {
    e <- ggplot(pd, aes(x = gids_scaled, y = volts)) +
      theme(palette.colour.continuous = "Okabe-Ito") +
      geom_point(aes(colour = pack_amps))
  } else {
    e <- ggplot(pd, aes(x = gids_scaled, y = volts)) +
      theme(palette.colour.continuous = "Okabe-Ito") +
      geom_point(aes(colour = pack_kW))
  }
  e + labs(
    title = paste0(
      m$name,
      ": from #",
      from_idx,
      " to #",
      to_idx,
      ". ",
      min_Hx,
      " ≤ Hx ≤ ",
      max_Hx,
      ", ",
      min_SOH,
      " ≤ SOH ≤ ",
      max_SOH,
      ",\n",
      round(min(pd$gids_scaled), 1),
      " ≤ GID/SOH ≤ ",
      round(max(pd$gids_scaled), 1),
      ", ", min(pd$min_cp_m_v / 1000),
      " ≤ Cell Voltage ≤ ",
      max(pd$max_cp_m_v / 1000)
    ),
    x = "GID / SOH",
    y = if (volts_per_cell)
      "Pack Voltage / 96"
    else
      "Pack Voltage"
  )
}
