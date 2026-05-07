#' concatenates csv files to create thmodel in .rda
Leaf24kWh_2020 <- thmodel_from_directory(
  logfildir = "data-raw/2020.Leaf.mdy",
  name = "Leaf24kWh_2020",
  USonian_dates = TRUE,
  model = "LEAF",
  capacity = 24) |> predict_temp()

usethis::use_data(Leaf24kWh_2020, overwrite = TRUE)
