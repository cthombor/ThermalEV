#' concatenates csv files to create thmodel in .rda
eNV200ac24kWh_2024 <- thmodel_from_directory(
  logfildir = "data-raw/2024.eNV.mdy",
  name = "ac24kWh_2024",
  USonian_dates = TRUE,
  capacity = 24) |> predict_temp()

usethis::use_data(eNV200ac24kWh_2024, overwrite = TRUE)
