## concatenates csv files pfrom Alistair's 24kWh e-NV200
## to create data/eNVa24kWh_2024.rda
## all have a working a/c

eNVa24kWh_2024 <- thmodel_from_directory(
  logfildir = "data-raw/2024a24kWh",
  name = "eNVa.24kWh.2024",
  capacity = 50) |> predict_temp()

usethis::use_data(eNVa24kWh_2024, overwrite = TRUE)
