#' plot_volts_ts: plot time-series of pack_volts, pred_pack_volts_from_soc

#' @param m a thmodel with predictions from predict_volts_tafel()
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#' @param volts_per_cell: T to divide pack_volts by the number of cell(pair)s
#'
#' @returns a ggplot() environment
#' @export
#'
#' @examples
#' plot_volts_ts(predict_volts_tafel(eNV50kWh))
plot_volts_ts <- function(m,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL,
                     min_sgids = NULL,
                     max_sgids = NULL,
                     volts_per_cell = F,
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

  missings <- is.na(pd$gids) |
    is.na(pd$pred_pack_volts_from_soc)
    is.na(pd$soc) |
    is.na(pd$pack_avg_temp) |
    is.na(pd$pack_amps) |
    is.na(pd$pack_volts) |
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
      missings) |>
    mutate(
      soc = soc / 1e6, # 0.0 to 1.0
      'volts - 350' = if (volts_per_cell)
        ((pack_volts - 350) / 96)
      else
        pack_volts - 350,
      #TODO: if volts_per_cell, change names so the legend isn't misleading
      'pred_volts - 350' = if (volts_per_cell)
        (pred_pack_volts_from_soc - 350) / 96
      else
        pred_pack_volts_from_soc - 350,
      amps = if (volts_per_cell)
        pack_amps / 96
      else
        pack_amps,
      'est. overvoltage * 10' = if (volts_per_cell)
        est_overvoltage * 10 / 96
      else est_overvoltage * 10
    )
  cat("Summary of soc:\n")
  print(summary(pd$soc))
  cat("Summary of corrected soc:\n")
  print(summary(pd$corr_soc))
  cat("Summary of estimated overvoltage at pack level:\n")
  print(summary(pd$est_overvoltage))

  pd <- pd |>
    select(date_time,
         'volts - 350',
         'pred_volts - 350',
         amps,
         'est. overvoltage * 10'
  ) |>
    xts::as.xts()

  plot(
    pd,
    legend.loc = "top",
    main.timespan = FALSE,
    format.labels = "%Y-%m-%d %H:%M",
    main = paste0(m$name)
  )

}
