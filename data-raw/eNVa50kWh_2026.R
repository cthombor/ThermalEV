## concatenates csv files pfrom Alistair's 50kWh e-NV200
## to create data/eNVa50kWh_2026.rda
## some have a working a/c

eNVa50kWh_2026 <- thmodel_from_directory(
  logfildir = "data-raw/2026a50kWh",
  name = "eNVa.50kWh.2026",
  capacity = 50) |> predict_temp()

usethis::use_data(eNVa50kWh_2026, overwrite = TRUE)
