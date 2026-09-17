## create a monster thmodel from my 24kWh Leaf thmodels

Leaf24kWh <- combine_thmodels(
  name = "Leaf24kWh",
  thmodels = list(Leaf24kWh_2019, Leaf24kWh_2021, Leaf24kWh_2022))

usethis::use_data(Leaf24kWh, overwrite = TRUE)
