# initial hacking on LeafSpy logs for thermal analysis

# globals
eff_pack_r <- 3.5 # nominal value, must be fitted to data
lambda <- 60 # thermal time constant for cell to pack (in seconds), fitted

sampling_interval <- 15 # nominal (in seconds), used in EMA filter
EMA_parameter <- sampling_interval / lambda # Exponential Moving Average
max_delta_t <- 33 # max seconds between samples in an analysed segment
# note: our predictor is not greatly affected by a single missing sample,
# but we start a new predictive sequence after multiple missing samples

# note: error in sampling_interval affects the fitted lambda, linearly
# error in the pack's heat capacity affects the fitted eff_pack_r, linearly

# 96 cells in the pack, 2.13 kg/cell
# water (4.13 J/Kg) in the electrolyte puts an upper-bound on cell heat capacity
pack_heat_capacity <- 96 * 2.13 * 1000 * 4.13
# the heat capacity of the 100kg of the non-cell pack contents is
# not quite negligible... but steel is only 0.5 J/Kg
pack_heat_capacity_from_steel <- 100 * 1000 * 0.5
# polyethylene is 2.0 J/Kg, everything other than water is lower than this
pack_heat_capacity_upper_bound <- pack_heat_capacity +
  pack_heat_capacity_from_steel * 2.0 / 0.5

# Run once:
# install.packages("tidyverse")
# install.packages("usethis")
# install.packages("xts")
# install.packages("janitor")

library(tidyverse)
library(usethis)
library(xts)
library(janitor)
# library(RcppRoll) # could be helpful for efficiency

# dates are in NZT from logfile; want display in NZT
# Sys.setenv(TZ = Sys.timezone())
# options(xts_check_TZ = FALSE)

# hacking with xts plots
soc_ts <- log26Jan26 |> select(date_time, soc) |> as.xts()
preds_ts <- cbind(pack_temp, pred_pack_temp, soc_ts)
plot(preds_ts, legend.loc = "right")

fit_ts <- logtibble |>
  select(
    date_time,
    pack_avg_temp,
    pred_pack_avg_temp,
    soc,
    pack_amps,
    charging_kW,
    pack_t1_c,
    pack_t2_c,
    pack_t4_c
  ) |>
  as.xts()

fit_tbl <- global_logtibble |>
  select(date_time,
         soc,
         pack_avg_temp,
         pred_pack_avg_temp,
         charging_kW)


