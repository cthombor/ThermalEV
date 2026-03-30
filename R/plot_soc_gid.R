#' plot_soc_gid: plot a computed SOC and the reported SOC against gid
#'
#' SOC drops from 100% to 0%, linearly in gids,
#' with 0% at min_gid.  At 100% SOC, the pack's usable capacity is
#' max_sgid * SOH.  When new (at 100% SOH), the pack's (nominal!)
#' usable capacity is max_sgid.
#'
#' We define a "scaled gid" as gid / SOH, and we use sgid to reference
#' this derived unit.  Note that this definition allows us to perform
#' a linear regression on the pack's reported gid and its reported SOC,
#' parameterised on the nominal original capacity of the pack, and
#' on the (presumably) invariant number of gids which are always
#' held in reserve, i.e. are unusable to drive the vehicle, but important
#' to mitigate the risk of "bricking" the pack if any cell's voltage
#' drops below its minimum-allowable value.
#'
#' It is widely believed that the Nissan BMS is designed to report its
#' best estimate of usable kWh in units (called the "gid" by the author
#' of LeafSpy) of 80 Wh.
#'
#' Any BMS *may* revise its current estimate of usable kWh, if its
#' estimate of the "resting voltage" of the pack, given its current
#' estimate of the average temperature of its cells, is inconsistent with
#' its current estimate for usable kWh.  There is normally a lookup
#' table in a BMS for this purpose, with entries derived from a
#' cell-manufacturer's spec sheet or from an empirical observation of
#' typical cell-behaviour in a current batch of cells.  I doubt any
#' automotive BMS will use a heuristic method to update its lookup
#' table based on the data it has collected from its pack's behaviour --
#' that could become quite unsafe!  Instead I think the method used
#' in a Nissan BMS is to make incremental corrections to its current estimate
#' for the gids remaining in the pack when there's a shift in its
#' temperature (and possibly in its average cell-voltage differential);
#' to adjust its estimated SOH (i.e. the pack's capacity) whenever
#' the pack has been discharged almost fully -- and perhaps the SOH is
#' adjusted at other times.
#'
#' The SOC as reported to me on my dashboard currently differ (by a few points)
#' to the SOC recorded in a LeafSpy log.  This is disturbing, but puzzling out
#' the relationship between these SOCs is I think best left until I have a good
#' understanding of how the SOC recorded in a LeafSpy log varies with the other
#' measurements and estimates recorded in this log.
#'
#' @param m a thmodel
#' @param max_sgids if !is.null, gids at 100% SOC and 100% SOH, else estimated
#' @param min_gids if !is.null, the (unscaled!) gids "in reserve" at 0% SOC
#' @param suppress_outliers if TRUE, don't plot the reported outliers
#' @param from_date starting date/time for plotting & analysis
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#' @param min_soc_filter high-pass filter on soc (to examine non-linearity)
#' @param max_soc_filter low-pass filter on soc (to examine non-linearity)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_soc_gid(eNV200ac24kWh_2025)
#' plot_soc_gid(eNV200ac24kWh_2025, max_soc = 25)
plot_soc_gid <- function(m,
                     max_gids = NULL,
                     min_gids = NULL,
                     suppress_outliers = FALSE,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL,
                     min_soc_filter = NULL,
                     max_soc_filter = NULL)
{
  pd <- m$logdata |>
    select(date_time, gids, soc, soh, pack_volts, a_hr, pack_amps,
           pack_avg_temp) |>
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
                       plotdata$date_time >= as.POSIXct(from_date)
                     )))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                   dplyr::last(which(
                     plotdata$date_time <= as.POSIXct(to_date)
                   )))
  pd <- pd |> slice(from_idx:to_idx)

  if (!is.null(max_soc_filter))
    pd <- pd[(pd$soc <= max_soc_filter), ]
  if (!is.null(min_soc_filter))
    pd <- pd[(pd$soc >= min_soc_filter), ]

  stopifnot(nrow(pd) > 0)

  # The mapping of gid onto SOC is nonlinear in SOH, but its variance is small
  # unless the pack is under heavy high-temperature load or is nearly turtled
  # (in which case the SOH may vary significantly, as it is updated by the BMS).
  # We warn in such cases.
  median_soh <- median(pd$soh, na.rm = TRUE)
  max_soh <- max(pd$soh, na.rm = TRUE)
  min_soh <- min(pd$soh, na.rm = TRUE)
  if (max_soh - min_soh > 5) {
    warning(paste0("SOH varies by more than 5 points, from ",
            min_soh, "% to ", max_soh, "%"))
  }

  if (is.null(max_gids)) { # regression
    mod <- lm(pd$soc ~ pd$gids_scaled)
    min_s <- mod$coefficients["(Intercept)"]
    slope_s <- mod$coefficients["pd$gids_scaled"]
    min_g <- - (median_soh / 100) * min_s / slope_s
    max_sg <- (min_g + (median_soh / 100) * (100 / slope_s)) /
      (median_soh / 100)
  } else { # compute from args
    # note that min_g is scaled, to maintain a reserve of constant size
#   min_g <- min_gids / (median_soh / 100)
#   max_g <- max_gids / (median_soh / 100)
    min_g <- min_gids
    max_sg <- max_sgids
    slope_s <- 100 / (max_sgids / (median_soh / 100) - min_g)
    min_s <- min_g * slope_s / (median_soh / 100)
  }

  pd <- pd |> mutate(computed_soc = min_s +
                       slope_s * gids / soh * 100)
  ggplot(pd, aes(x = gids, y = soc)) +
    labs(y = "LeafSpy SOC") +
    geom_point() +
    labs(
      title = paste0(
        m$name,
        ": from #",
        from_idx,
        " to #",
        to_idx,
        ifelse(!is.null(min_soc_filter),
               paste0("; SOC ≥ ", min_soc_filter),
               ""),
        ifelse(!is.null(max_soc_filter),
               paste0("; SOC ≤ ", max_soc_filter),
               "")
      ),
      subtitle = paste0(
        ifelse(is.null(max_gids),
               "Best-fit SOC = ",
               "Computed SOC = "),
        round(min_s, 1),
        " + ",
        round(slope_s, 3),
        " * gids / SOH; min_gids = ",
        round(min_g, 1),
        ", max_sgid = ",
        round(max_sg, 1),
        ", SOH ~ ",
        round(median_soh, 0)
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
