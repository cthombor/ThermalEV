#' plot_gid_kWh: gids*.080, estimated kWh, temps, inefficiencies
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
#' plot_gid_kWh(eNV200ac24kWh_2025)
plot_gid_kWh <- function(m,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL)
{
  pd <- m$logdata |>
    select(date_time,
           gids,
           soc,
           soh,
           obc_out_pwr,
           pack_volts,
           pack_amps,
           pack_avg_temp,
           delta_t,
           waste_heatJ_kWh,
           AC_energy_kWh) |>
    mutate(
      gids_scaled = gids / (soh / 100),
      'Volts - 340' = pack_volts - 340,
      soc = soc / 1e4, # BMS scaling
      charging_kW = obc_out_pwr / 1000,
      kWh_remaining = gids * 0.080, # assuming 1 gid = 80 Wh
      grp_num = cumsum(is.na(delta_t)),
      adj_delta_t = ifelse(is.na(delta_t), 15, delta_t)
    ) |>
    mutate(
      delta_kWh = - pack_amps * pack_volts * adj_delta_t / 3600 / 1000,
      delta_kWh2 = charging_kW * adj_delta_t / 3600
    ) |>
    arrange(date_time)

  pd <- pd |>
    group_by(grp_num) |>
    mutate(cumsum_delta_kWh = cumsum(delta_kWh),
           cumsum_delta_kWh2 = cumsum(delta_kWh2),
           # restart these accumulators in each group
           waste_heatJ_kWh =  waste_heatJ_kWh - first(waste_heatJ_kWh),
           AC_energy_kWh = AC_energy_kWh - first(AC_energy_kWh)
           ) |>
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

  if (is.na(from_idx) || is.na(to_idx)) {
    warning("Date out of range")
  } else if (from_idx >= to_idx) {
    warning("from_date is not before to_date")
  }

  pd <- pd |>
    slice(from_idx:to_idx) |>
    mutate('gids * 0.08' = gids * 0.08,
           'LeafSpy %SOC * nom_kWh * %SOH' =
             (soc / 100) * m$capacity * (soh / 100) )

  if (nrow(pd) == 0) {
    warning("No data to plot!")
  }

  lcdk <- last(pd$cumsum_delta_kWh)
  lcdk2 <- last(pd$cumsum_delta_kWh2)
  lwhk <- last(pd$waste_heatJ_kWh)
  lACe <- last(pd$AC_energy_kWh)
  cat(round(lcdk, 3), "kWh to pack, ",
      round(lwhk, 3), "kWh Joule heating, ",
      round(lACe, 3), "kWh AC consumption\n")
  cat("Alternative estimation of charging kWh:", round(lcdk2,3), "\n")
  cat("kWh added: ", round((last(pd$gids) - first(pd$gids)) * 0.08, 2), "\n")
  cat("Estimated kWh added: ", round(lcdk - lwhk - lACe, 3), "\n")
  cat("Estimated efficiency: ", round((lcdk - lwhk - lACe) / lcdk, 3), "\n")

  pdts <- pd |>
    select(date_time,
           cumsum_delta_kWh,
           'gids * 0.08',
           'Volts - 340',
           'LeafSpy %SOC * nom_kWh * %SOH',
           pack_avg_temp,
           waste_heatJ_kWh,
           AC_energy_kWh,
           charging_kW
           ) |>
    as.xts()
  plot(pdts,
       type = "p",
       legend.loc = "top",
       main = m$name)

}
