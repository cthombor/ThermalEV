# notes on heat capacity, hacks and fiddles

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

#hacking with filters

  # we now apply a second exponential filter, with a much longer time
  # constant, to (very roughly) model the cooling of the pack by its
  # convection and conduction to the ambient air (and also to the
  # vehicle's frame, which is assumed to be in thermal equilibrium
  # with the ambient temperature as measured by a thermosensor at
  # the front of the vehicle.
  ambient_xts <- as.xts(logtibble$ambient, logtibble$date_time)
  EMA_parameter_pack_to_ambient <- sampling_interval / lambda_pack_to_ambient
  lagged_heat <- as.xts(vector(mode = "double", length = length(unlagged_heat)),
                        logtibble$date_time)
  for (i in seq(nsegments)[which(!wexclude)]) {
    # compute a lagged time-series of pack-to-ambient temperature differentials.
    # These are proportional to heats in J, with the constant of proportionality
    # being the heat capacity of the pack excluding the modules and their
    # contents.  If the high-amp cabling to the modules is heating
    # significantly, this wattage will be lumped with the Joule heating of the
    # cells, and the time-constant of its decay will be lumped with the (short)
    # time-constant of the modules' thermosensor coming into equilibrium with
    # its cells.
    starting_temp <- as.double(pack_avg_temp_xts[wstart[i]])
    lagged_heat[wstart[i]:wend[i]] <-
      stats::filter(
        (ambient_xts[wstart[i]:wend[i]] -
           pred_pack_avg_temp_xts[wstart[i]:wend[i]]) *
          EMA_parameter_pack_to_ambient,
        1. - EMA_parameter_pack_to_ambient,
        method = "recursive",
        init = starting_temp - ambient_xts[wstart[i]]
      )
  }
  pred_pack_avg_temp_xts <- pred_pack_avg_temp_xts + lagged_heat

  pred_pack_avg_temp_xts[segment_starts] <- NA # these aren't predictions


