## concatenates csv files in data-raw/eNV200ac24kWh_2025 to create
## data/eNV200ac24kWh_2025.rda

eNV200ac24kWh_2025 <- thmodel_from_directory(
  logfildir = "data-raw/2025.eNV",
  name = "ac24kWh_2025",
  capacity = 24) |> predict_temp()

usethis::use_data(eNV200ac24kWh_2025, overwrite = TRUE)
