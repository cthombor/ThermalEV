#' plot_gids: scatterplot of SOC and pack_volts v gids, also a timeseries plot
#'
#' @param m a thmodel with temperature predictions
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#' @param min_soc high-pass filter on soc (to examine non-linearity)
#' @param max_soc low-pass filter on soc (to examine non-linearity)
#' @param suppress_outliers TRUE by default: outliers are not plotted
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_gids(eNV200ac24kWh_2025)
#' plot_gids(eNV200ac24kWh_2025, min_soc = 70)
plot_gids <- function(m,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL,
                     min_soc = NULL,
                     max_soc = NULL,
                     suppress_outliers = TRUE)
{
  pd <- m$logdata |>
    select(date_time, gids, soc, soh, pack_volts, a_hr, pack_amps) |>
    mutate(gids_scaled = gids / (soh / 100),
           soc = soc / 1e4,
           a_hr = a_hr / 1e4,
           volts_scaled = pack_volts - 300) |>
    mutate(volts_scaled = ifelse(volts_scaled < 0, NA, volts_scaled)) |>
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

  pd <- pd |> slice(from_idx:to_idx)
  if (!is.null(max_soc))
    pd <- pd[(pd$soc <= max_soc), ]
  if (!is.null(min_soc))
    pd <- pd[(pd$soc >= min_soc), ]

  if (nrow(pd) == 0) {
    warning("No data to plot!")
  }

  print("Ratio of gids/soh to soc:")
  print(summary(pd$gids_ratio))
  meanrat <- mean(pd$gids_ratio)
  outliers <- abs(meanrat * pd$soc - pd$gids_scaled) > 50
  if (any(outliers))
    warning(paste(c("Outliers at index #",
                    which(outliers),
                    ifelse(suppress_outliers, "not plotted", "")),
                  collapse = " "))
  if (suppress_outliers) pd <- pd[!outliers, ]

  # dead code, maybe useful some day to visualise scaled gids,
  # soc, soh, volts_scaled, a_hr, pack_amps
  if (FALSE) {
    pdts <- pd |> select(!pack_volts) |> select(!gids) |> as.xts()
    plot(pdst, type="p", legend.loc = "top")
  }

  mod <- lm(soc ~ gids_scaled, pd)
  print(paste("SOC/scaled_gid slope =", round(mod$coefficients[2], 3),
              "; SOC intercept =", round(mod$coefficients[1], 1)))

  ggplot(pd, aes(x = gids_scaled, y = soc)) +
    labs(title = paste0(m$name, ": from #", from_idx, " to #", to_idx,
                        ifelse(!is.null(min_soc),
                               paste0("; SOC ≥ ", min_soc), ""),
                        ifelse(!is.null(max_soc),
                               paste0("; SOC ≤ ", max_soc), "")),
         subtitle = paste0("Linear regression: SOC = ",
                           round(mod$coefficients[1], 1),
                           " + ",
                           round(mod$coefficients[2], 3),
                           " * gid / SOH",
                           ifelse(suppress_outliers & any(outliers),
                                  paste0("; ",
                                         sum(outliers, na.rm=TRUE),
                                         " outliers removed"),
                                  "")
                           )
         ) +
    geom_point()
}
