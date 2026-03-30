## concatenates csv files in data-raw/2024.eNV.mdy to create
## data/eNV200ac24kWh_2024mdy.rda

eNV200ac24kWh_2024mdy <- thmodel_from_directory(
  logfildir = "data-raw/2024.eNV.mdy",
  name = "ac24kWh_2024.mdy",
  USonian_dates = TRUE,
  capacity = 24) |> predict_temp()

usethis::use_data(eNV200ac24kWh_2024mdy, overwrite = TRUE)
