# hacking with xts plots, notes on heat capacity, fiddles with Sys.timezone()

# 96 cells in the pack, 2.13 kg/cell
# water (4.13 J/gK) in the electrolyte puts an upper-bound on cell heat capacity
pack_heat_capacity <- 96 * 2.13 * 1000 * 4.13
# the heat capacity of the 100kg of the non-cell pack contents is
# not quite negligible... but steel is only 0.5 J/gK
pack_heat_capacity_from_steel <- 100 * 1000 * 0.5
# polyethylene is 2.0 J/gK, everything other than water is lower than this
pack_heat_capacity_upper_bound <- pack_heat_capacity +
  pack_heat_capacity_from_steel * 2.0 / 0.5

library(tidyverse)
library(xts)

# experimenting with Sys.timezone() in xts(), maybe a good way to mute timezone
# warning messages in xts?  Deep waters here, because the Win32 runtime doesn't
# support the *nix-style client-server distinction... so a remote R server's
# behaviour might be surprising with respect to its default TZ for xts...
# Sys.setenv(TZ = Sys.timezone()) # avoid warnings?
# options(xts_check_TZ = FALSE) # disable warnings?

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

