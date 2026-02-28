# I'm curious to see if pack voltage is similar to sum of abs(cp*)/1000.

# Conclusion: yes, they differ only in noise: a few volts when driving,
# millivolts while fastcharging.

# The difference (here called harness_drop) isn't correlated with pack_amps,
# so some factor other than amperage is causing pack_volts to diverge from
# the sum of the measured cell-voltages. Possibly: shunt activity disturbs
# this measurement.

library(tidyverse)
library(janitor)

log26Jan26 <- read_csv("data-raw/26Jan26.csv",
                       col_types =
                         cols(`Date/Time` =
                                col_datetime("%d/%m/%Y %H:%M:%S")))
log26Jan26 <- log26Jan26 |> janitor::clean_names()

log26Jan26 <- log26Jan26 |> rowwise() |>
  mutate(sum_cp = sum(abs(c_across(cp1:cp96))))
log26Jan26$sum_cp[1:2]
summary(log26Jan26$sum_cp)

#so many columns, let's add some more (by parsing cp*)
cps <- log26Jan26 |>
  select(cp1:cp96)
acps <- cps |>
  abs() |>
  rename_with(stringr::str_replace,
              pattern = "cp",
              replacement = "acp")
cp_shunts <- cps |>
  sign() |>
  mutate_all((\(x) if_else(x == -1, TRUE, FALSE))) |>
  rename_with(stringr::str_replace,
              pattern = "cp",
              replacement = "cp_shunt")
# delete the cp*; add the (parsed) acp* and cp_shunt*
log26Jan26 <- log26Jan26 |> select(!starts_with("cp"))
log26Jan26 <- cbind(log26Jan26, acps, cp_shunts)
rm(acps,cp_shunts,cps)

# total voltage across cells (in V)
log26Jan26 <- log26Jan26 |>
  mutate(sum_cp = rowSums(across(acp1:acp96)) / 1000.0,
         .before = acp1)
# this works, but is inefficient compared to rowSums
# log26Jan26 <- log26Jan26 |> rowwise() |>
#   mutate(sum_cp = sum(abs(c_across(cp1:cp96))))

plot(log26Jan26$pack_volts - log26Jan26$sum_cp/1000.)

# harness drop (in V)
log26Jan26 <- log26Jan26 |>
  mutate(harness_drop = sum_cp - pack_volts,
         .before = acp1)

cat("cor(harness_drop, pack_amps) =",
    cor(log26Jan26$harness_drop, log26Jan26$pack_amps))
