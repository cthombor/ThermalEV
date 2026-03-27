## code to prepare `eNV200noac50kWh` dataset goes here

eNV200ac50kWh <- thmodel_from_directory(
  logfildir = "data-raw/2026.50kWh.ac",
  name = "ac50kWh",
  capacity = 50) |> predict_temp()

usethis::use_data(eNV200ac50kWh, overwrite = TRUE)
