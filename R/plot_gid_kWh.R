#' plot_gid_kWh: explore kWh v gid
#'
#' @param m a thmodel with temperature preductions
#' @param Wh_per_gid adjustable, for empirical confirmation
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_gid_kWh(predict_temp(eNV200ac24kWh_2025))
plot_gid_kWh <- function(m,
                         Wh_per_gid = 80,
                         from_date = NULL,
                         to_date = NULL,
                         from_idx = NULL,
                         to_idx = NULL)
{
  pd <- m$logdata |>
    select(date_time, gids, soc, soh, a_hr, pack_volts, pack_amps, delta_t,
           pack_avg_temp)

  # it's impossible to add multiple gids to a pack in a 15s interval; but gids
  # are sometimes recalibrated (e.g. when the SOC is very low)
  wonky_data <- (abs(pd$gids - lag(pd$gids)) > 1) & (!is.na(pd$delta_t))
  wct <- sum(wonky_data, na.rm = TRUE)
  if (wct > 0) {
    pd <- pd |>
      mutate(delta_t = ifelse(wonky_data, NA, delta_t))
  }

  pd <- pd |>
    mutate(
      grp_num = cumsum(is.na(delta_t)),
      adj_delta_t = ifelse(is.na(delta_t), 15, delta_t),
      # avoid difficulty with NA in a cumsum()
      pack_volts = ifelse(is.na(pack_volts),
                          max(lead(pack_volts), lag(pack_volts), na.rm = TRUE),
                          pack_volts),
      'Volts - 340' = pack_volts - 340,
      reported_soc = soc / 1e4,
      a_hr = a_hr / 1e4,
      kWh_from_gids = Wh_per_gid * gids / 1000,
    ) |>
    mutate(
      kW = pack_volts * pack_amps / 1000,
      delta_kWh = - kW * adj_delta_t / 3600,
    ) |>
    mutate( # initialise the kWh accumulator for each group
      delta_kWh = ifelse(is.na(delta_t), kWh_from_gids, delta_kWh)
    ) |>
    arrange(date_time)

  pd <- pd |>
    group_by(grp_num) |>
    mutate(cumsum_delta_kWh = cumsum(delta_kWh)) |>
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

  pd <- pd |>
    slice(from_idx:to_idx) |>
    mutate(pred_kWh = ifelse(is.na(delta_t),
           NA,
           cumsum_delta_kWh))

  wonky_data <- wonky_data[from_idx:to_idx]
  wct <- sum(wonky_data, na.rm = TRUE)
  warning(
    wct,
    " wonky/recalibrated gid readings will restart kWh estimations.",
    " First: ",
    pd$date_time[first(which(wonky_data))]
  )

  if (nrow(pd) == 0) {
    warning("No data to plot!")
  }

  pdts <- pd |>
    select(date_time,
           kWh_from_gids, pred_kWh, 'Volts - 340',
           pack_avg_temp) |>
    as.xts()
  plot(
    pdts,
    type = "p",
    legend.loc = "top",
    main =
      paste0(
        m$name,
        ": ",
        Wh_per_gid,
        " Wh/gid, corr = ",
        round(cor(pd$pred_kWh,
                  pd$kWh_from_gids,
                  use="complete.obs"), 3)
      )
  )
}
