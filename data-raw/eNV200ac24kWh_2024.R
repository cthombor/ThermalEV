## concatenates csv files in data-raw/2024.eNV to create
## data/eNV200ac24kWh_2024.rda

## n.b. all datestamps are usonian in this directory

eNV200ac24kWh_2024 <- thmodel_from_directory(
  logfildir = "data-raw/2024.eNV",
  name = "ac24kWh_2024",
  USonian_dates = TRUE,
  capacity = 24) |> predict_temp()

usethis::use_data(eNV200ac24kWh_2024, overwrite = TRUE)
