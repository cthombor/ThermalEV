#' concatenates csv files to create thmodel in .rda
Leaf24kWh_2022 <- thmodel_from_directory(
  logfildir = "data-raw/2022.Leaf.mdy",
  name = "Leaf24kWh_2022",
  USonian_dates = TRUE,
  capacity = 24) |> predict_temp()

usethis::use_data(Leaf24kWh_2022, overwrite = TRUE)
