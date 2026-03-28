#' plot_soc_gid: plot a computed SOC and the reported SOC against gid
#'
#' @param m a thmodel
#' @param max_gids if !is.null, the gids at 100% SOC; else estimate from m
#' @param reserve_gids if !is.null, the fraction of max_gids remaining at 0% SOC
#' @param suppress_outliers if TRUE, won't plot the reported outliers
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#' @param min_soc high-pass filter on soc (to examine non-linearity)
#' @param max_soc low-pass filter on soc (to examine non-linearity)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_soc_gid(eNV200ac24kWh_2025)
#' plot_soc_gid(eNV200ac24kWh_2025, min_soc = 70)
plot_soc_gid <- function(m,
                     max_gids = NULL,
                     reserve_gids = NULL,
                     suppress_outliers = FALSE,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL,
                     min_soc = NULL,
                     max_soc = NULL)
{
  pd <- m$logdata |>
    select(date_time, gids, soc, soh, pack_volts, a_hr, pack_amps) |>
    mutate(
      gids_scaled = gids / (soh / 100),
      soc = soc / 1e4,
      a_hr = a_hr / 1e4,
      volts_scaled = pack_volts - 300
    ) |>
    mutate(volts_scaled = ifelse(volts_scaled < 0, NA, volts_scaled)) |>
    arrange(date_time)

  # curiously, xts insists on UTC for all stored time/date data. Our goal is to
  # interpret from_date and to_date consistently with the x-axis-labels of
  # plot.xts, and we refuse to display date/times that are actually in UTC. What
  # a muddle!

  # Warning: the date/time comparisons below are hazardous, as they may depend
  # on difficult-to-discover environment variables (such as the *nix-ish TZ,
  # which doesn't have an exact translation in a Win11 environment due to its
  # weak support for client-server computing).  They may also depend on the
  # order of loading library(tidyverse) and library(xts).  What a muddle!
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

  stopifnot(nrow(pd) > 0)

  # the mapping is nonlinear in SOH, but its variance should be small
  median_soh <- median(pd$soh, na.rm = TRUE)
  max_soh <- max(pd$soh, na.rm = TRUE)
  min_soh <- min(pd$soh, na.rm = TRUE)
  if (max_soh - min_soh > 5) {
    warning(paste0("SOH varies by more than 5 points, from ",
            min_soh, "% to ", max_soh, "%"))
  }

  if (is.null(max_gids)) {
    mod <- lm(pd$soc ~ pd$gids_scaled)
    min_gids <- mod$coefficients["(Intercept)"] / (median_soh / 100)
    slope_soc <- mod$coefficients["pd$gids_scaled"]
    max_gids <- min_gids + 100 / slope_soc
  }

  # SOC drops from 100% to 0%, linearly in gids, with 0% at min_gids
  # 1 gid = 80 kWh

  pd <- pd |> mutate(computed_soc =
                       min_gids +
                       slope_soc *
                         (max_gids - min_gids) / max_gids *
                         (gids / soh * 100))
  ggplot(pd, aes(x = gids, y = soc, label = "BMS SOC")) +
    labs(y = "SOC") +
    geom_point() +
    labs(
      title = paste0(
        m$name,
        ": from #",
        from_idx,
        " to #",
        to_idx,
        ifelse(!is.null(min_soc), paste0("; SOC ≥ ", min_soc), ""),
        ifelse(!is.null(max_soc), paste0("; SOC ≤ ", max_soc), "")
      ),
      subtitle = paste0(
        "Computed soc = ",
        round(min_gids, 1),
        " + ",
        round(slope_soc * (max_gids - min_gids) / max_gids, 3),
        " * gids / SOH"
      )
    ) +
    geom_line(
      data = pd,
      colour = "red",
      mapping = aes(x = gids, y = computed_soc)
    ) +
    theme(legend.position = "none") +
    theme(plot.subtitle=element_text(color="red"))
}
