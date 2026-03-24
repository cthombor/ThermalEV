#' plot_gid_ahr: explore ahr v gid
#'
#' Plots reveal that gid is a biased estimate of Ah remaining, with
#' a multiplicative factor of pack_volts explaining much of the variance.
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
#' plot_gid_ahr(eNV200ac24kWh_2025)
#' plot_gid_ahr(eNV200ac24kWh_2025, min_soc = 70)
plot_gid_ahr <- function(m,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL)
{
  pd <- m$logdata |>
    select(date_time, gids, soc, soh, a_hr, pack_volts, pack_amps, delta_t) |>
    mutate(
      gids_scaled = gids / (soh / 100),
      'Volts - 340' = pack_volts - 340,
      soc = soc / 1e4,
      a_hr = a_hr / 1e4,
      ah_remaining = gids * 0.18,
      grp_num = cumsum(is.na(delta_t)),
      adj_delta_t = ifelse(is.na(delta_t), 15, delta_t)
    ) |>
    mutate(
      delta_ah = - pack_amps * adj_delta_t / 3600,
    ) |>
    mutate(
      delta_ah = delta_ah + ifelse(is.na(delta_t), ah_remaining, 0)
    ) |>
    arrange(date_time)

  pd <- pd |>
    group_by(grp_num) |>
    mutate(cumsum_delta_ah = cumsum(delta_ah)) |>
    ungroup()

  # curiously, xts insists on UTC for stored dates & times
  from_idx <- ifelse(is.null(from_date),
                     ifelse(is.null(from_idx), 1, from_idx),
                     dplyr::first(which(
                       pd$date_time >= as.POSIXct(from_date, tz = "UTC")
                     )))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                   dplyr::last(which(
                     pd$date_time <= as.POSIXct(to_date, tz = "UTC")
                   )))

  pd <- pd |> slice(from_idx:to_idx)

  if (nrow(pd) == 0) {
    warning("No data to plot!")
  }

  pdts <- pd |>
    select(date_time, cumsum_delta_ah, ah_remaining, 'Volts - 340') |>
    as.xts()
  plot(pdts, type = "p", legend.loc = "top")

}
