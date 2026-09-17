#' plot_gids: scatterplot of SOC and pack_volts v gids
#'
#' @param m a thmodel with temperature predictions
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#' @param min_soc high-pass filter on soc (to examine non-linearity)
#' @param max_soc low-pass filter on soc (to examine non-linearity)
#' @param from_temp lower limit of battery temps to be analysed
#' @param to_temp upper limit of battery temps to be analysed
#' @param suppress_outliers FALSE by default: outliers are not plotted
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
                     from_temp = NULL,
                     to_temp = NULL,
                     suppress_outliers = FALSE)
{
  pd <- m$logdata |>
    select(date_time, gids, soc, soh, pack_volts,
           a_hr, pack_amps, pack_avg_temp) |>
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

  pd <- pd |> slice(from_idx:to_idx)

  min_soc <- if (is.null(min_soc)) 0 else min_soc
  max_soc <- if (is.null(max_soc)) 100 else max_soc
  extreme_socs <- (pd$soc < min_soc) | (pd$soc > max_soc)

  min_temp <- if (is.null(from_temp)) -30 else from_temp
  max_temp <- if (is.null(to_temp)) 100 else to_temp
  extreme_temps <- (pd$pack_avg_temp < min_temp) | (pd$pack_avg_temp > max_temp)

  cat("Filtering out",
      sum(extreme_socs, na.rm = TRUE), "extreme-soc records, and",
      sum(extreme_temps, na.rm = TRUE), "extreme-temp records\n")
  pd <- pd |>
    filter_out(extreme_socs | extreme_temps)

  if (nrow(pd) == 0) {
    warning("No data to plot!")
  }

  cat("Ratio of gids/soh to soc:")
  print(summary(pd$gids_ratio))
  meanrat <- mean(pd$gids_ratio, na.rm=TRUE)
  outliers <- abs(meanrat * pd$soc - pd$gids_scaled) > 50
  if (any(which(outliers)))
    warning(paste(c("Outliers at index #",
                    which(outliers),
                    ifelse(suppress_outliers, "not plotted", "")),
                  collapse = " "))
  if (suppress_outliers) pd <- pd[!outliers, ]

  mod <- lm(soc ~ gids_scaled, pd)
  cat(paste("SOC/scaled_gid slope =", round(mod$coefficients[2], 3),
              "; SOC intercept =", round(mod$coefficients[1], 1)))

  pd <- pd |> mutate(
    temps = as_factor(round(pack_avg_temp / 10, 0) * 10))
  suppressWarnings(
    pd <- pd |> mutate(
      temps = fct_recode(
        temps,
        "< 5\u2009°C" = "0",
        "[5, 15)\u2009°C" = "10",
        "[15, 25)\u2009°C" = "20",
        "[25, 35)\u2009°C" = "30",
        "> 35\u2009°C" = "40")))
  mycolors = c("< 5\u2009°C"  = "violet",
               "[5, 15)\u2009°C" = "blue",
               "[15, 25)\u2009°C" = "green",
               "[25, 35)\u2009°C" = "orange",
               "> 35\u2009°C" = "red")

  ggplot(pd, aes(x = gids_scaled, y = soc)) +
    labs(title = paste0(m$name, ": from #", from_idx, " to #", to_idx,
                        ifelse(!is.null(min_soc) && min_soc > 0,
                               paste0("; SOC ≥ ", min_soc), ""),
                        ifelse(!is.null(max_soc) && max_soc < 100,
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
    geom_point(aes(colour = temps)) +
    scale_color_manual(values=mycolors)
}
