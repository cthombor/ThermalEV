## code to prepare `eNV200noac50kWh` dataset goes here

eNV200noac50kWh <- thmodel_from_directory(
  logfildir = "data-raw/eNV200noac50kWh",
  name = "noac50kWh",
  capacity = 50)

usethis::use_data(eNV200noac50kWh, overwrite = TRUE)
