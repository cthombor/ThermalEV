## code to prepare `eNV200noac50kWh` dataset goes here

eNV200noac50kWh_2025 <- thmodel_from_directory(
  logfildir = "data-raw/eNV200noac50kWh",
  name = "noac50kWh")

usethis::use_data(eNV200noac50kWh, overwrite = TRUE)
