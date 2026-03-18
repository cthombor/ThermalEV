#' plot_gids: scatterplot of pack_volts v scaled gids
#'
#' @param m a thmodel
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#' @param max_sgids low-pass filter on scaled gids (to examine non-linearity)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_volts(eNV200ac24kWh_2025)
#' plot_volts(eNV200ac24kWh_2025, min_sgids = 25)
plot_volts <- function(m,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL,
                     max_sgids = NULL)
{
  pd <- m$logdata |>
    select(date_time, gids, soc, soh, pack_volts) |>
    mutate(gids_scaled = gids / (soh / 100),
           soc = soc / 1e4) |>
    mutate(pack_volts = ifelse(pack_volts == 0, NA, pack_volts)) |>
    mutate(gids_scaled = ifelse(gids_scaled == 0, NA, gids_scaled)) |>
    mutate(gids_ratio = gids_scaled / soc) |>
    arrange(date_time)

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
  if (is.null(from_idx) || is.null(to_idx) || from_idx > to_idx) {
    warning("No data to plot!")
  }

  pd <- pd |> slice(from_idx:to_idx)
  if (!is.null(max_sgids))
    pd <- pd[(pd$gids_scaled <= max_sgids), ]

  ggplot(pd, aes(x=gids_scaled, y=pack_volts)) +
    geom_point() +
    labs(title = paste0(m$name, ": from #", from_idx, " to #", to_idx))

}
