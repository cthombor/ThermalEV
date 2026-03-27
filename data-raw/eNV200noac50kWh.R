## code to prepare `eNV200noac50kWh` dataset goes here

eNV200noac50kWh <- thmodel_from_directory(
  logfildir = "data-raw/2026.50kWh.noac",
  name = "noac50kWh",
  capacity = 50) |> predict_temp()

usethis::use_data(eNV200noac50kWh, overwrite = TRUE)
