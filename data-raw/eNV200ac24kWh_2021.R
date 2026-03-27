#' concatenates csv files to create thmodel in .rda
eNV200ac24kWh_2021 <- thmodel_from_directory(
  logfildir = "data-raw/2021.eNV.mdy",
  name = "ac24kWh_2021",
  USonian_dates = TRUE,
  capacity = 24) |> predict_temp()

usethis::use_data(eNV200ac24kWh_2021, overwrite = TRUE)
