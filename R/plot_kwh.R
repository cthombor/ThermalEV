#' plot_kWh: explore gid/soh v a numeric integration of pack VA.
#'
#' ToDo: Nominally, 1 gid/SOH = 80 kWh, but we could estimate this ratio for
#' any pack by analysing the evolution of its reported gids with
#' a numerical integration of observations of (pack volts)*(pack amps).
#'
#' @param m a thmodel with temperature preductions
#' @param Wh_per_gid adjustable, for empirical confirmation
#' @param eff_resistance adjustable, for Joule-heat correction, in milliohms
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_kWh(predict_temp(eNV200ac24kWh_2025))
plot_kWh <- function(m,
                     Wh_per_gid = 80,
                     eff_resistance = 500,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL)
{
  pd <- m$logdata |>
    select(date_time, gids, soc, soh, a_hr, pack_volts, pack_amps,
           delta_t, est_pwr_a_c_50w, aux_pwr_100w, est_pwr_htr_250w,
           pack_avg_temp)

  # it's impossible to add many gids to a pack in a 15s interval, even when
  # charging at 48kW (which seems to be the limiting rate for a charge or
  # discharge on a Leaf or e-NV200); but gids are sometimes recalibrated (e.g.
  # when the SOC is very low).
  intervals_per_hour = 60 * median(pd$delta_t, na.rm = TRUE)
  max_delta_g <- ceiling((48000 / Wh_per_gid) / intervals_per_hour)
  wonky_data <-
    (abs(pd$gids - lag(pd$gids)) > max_delta_g) & (!is.na(pd$delta_t))
  # wct <- sum(wonky_data, na.rm = TRUE)
  pd <- pd |>
    mutate(
      delta_t = ifelse(wonky_data, NA_real_, delta_t),
      recalib = ifelse(wonky_data, pd$gids - lag(pd$gids), 0)
    ) |>
    mutate(
      grp_num = cumsum(is.na(delta_t)),
      adj_delta_t = ifelse(is.na(delta_t), 15, delta_t),
      # avoid difficulty with NA in a cumsum()
      pack_volts = ifelse(is.na(pack_volts),
                          median(lead(pack_volts),
                                 lag(pack_volts),
                                 na.rm = TRUE),
                          pack_volts),
      'Volts - 340' = pack_volts - 340,
      'Amps' = pack_amps,
      reported_soc = soc / 1e4,
      a_hr = a_hr / 1e4,
      kWh_from_gids = Wh_per_gid * gids / 1000,
    ) |>
    mutate(
      kW = pack_volts * pack_amps / 1000,
      delta_kWh = - kW * adj_delta_t / 3600 -
        pack_amps * pack_amps * eff_resistance / 1e6 -
        0.1 * aux_pwr_100w -
        0.05 * est_pwr_a_c_50w -
        0.25 * est_pwr_htr_250w
    ) |>
    mutate( # initialise the kWh accumulator for each group
      delta_kWh = ifelse(is.na(delta_t), kWh_from_gids, delta_kWh)
    ) |>
    arrange(date_time)

  pd <- pd |>
    group_by(grp_num) |>
    mutate(cumsum_delta_kWh = cumsum(delta_kWh)) |>
    ungroup()

  from_idx <- ifelse(is.null(from_date),
                     ifelse(is.null(from_idx), 1, from_idx),
                     dplyr::first(which(
                       pd$date_time >= as.POSIXct(from_date, tz = "NZ")
                     )))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                   dplyr::last(which(
                     pd$date_time <= as.POSIXct(to_date, tz = "NZ")
                   )))

  pd <- pd |>
    slice(from_idx:to_idx) |>
    # repair typecasting errors, an ineffective hack
    # mutate(
    #  cumsum_delta_kWh =
    #    ifelse(is.na(cumsum_delta_kWh), NA_real_, cumsum_delta_kWh),
    #  kWh_from_gids =
    #    ifelse(is.na(kWh_from_gids), NA_real_, kWh_from_gids)
    # ) |>
    mutate(
      'kWh from gids' = kWh_from_gids,
      'Pred kWh' = ifelse(is.na(delta_t),
                          NA_real_,
                          cumsum_delta_kWh),
      'Pack avg temp' = pack_avg_temp,
      'Cumsum gids recalib' = cumsum(recalib))

  # a successful hack.
  x <- pd$'Pred kWh'
  y <- pd$'kWh from gids'
  prederror <- x - y

  pd <- pd |>
    mutate(
      # the following throws a "non-numeric argument to binary operator" error
      # 'Pred error' = 'Pred kWh' - 'kWh from gids')
      'Pred error' = prederror)

  wonky_data <- wonky_data[from_idx:to_idx]
  # wct <- sum(wonky_data, na.rm = TRUE)

  stopifnot(nrow(pd) > 0)

  mod <- lm(pd$cumsum_delta_kWh ~ pd$kWh_from_gids)

  pdts <- pd |>
    select(date_time,
           'Amps',
           'Pack avg temp',
           est_pwr_a_c_50w,
           aux_pwr_100w,
           est_pwr_htr_250w) |>
    as.xts()
  plot(
    pdts,
    type = "p",
    legend.loc = "top",
    main =
      paste0(m$name, "\n", Wh_per_gid, " Wh/gid, median error = ",
             round(median(prederror, na.rm=TRUE), 3)
             )
  )

  pdts2 <- pd |>
    select(date_time,
           'Pred error',
           'Cumsum gids recalib') |>
    as.xts()
  addSeries(
    pdts2,
    type = "p",
    legend.loc = "top"
  )

}
