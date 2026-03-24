## code to prepare `eNV200noac50kWh` dataset goes here

eNV200ac50kWh <- thmodel_from_directory(
  logfildir = "data-raw/eNV200ac50kWh",
  name = "ac50kWh")

usethis::use_data(eNV200ac50kWh, overwrite = TRUE)
