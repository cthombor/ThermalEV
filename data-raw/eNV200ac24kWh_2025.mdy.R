## concatenates csv files in data-raw/2025.eNV.mdy to create
## data/eNV200ac24kWh_2025mdy.rda

eNV200ac24kWh_2025mdy <- thmodel_from_directory(
  logfildir = "data-raw/2025.eNV.mdy",
  name = "ac24kWh_2025",
  USonian_dates = TRUE,
  capacity = 24) |> predict_temp()

usethis::use_data(eNV200ac24kWh_2025mdy, overwrite = TRUE)
