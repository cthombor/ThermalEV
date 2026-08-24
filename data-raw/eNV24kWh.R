## create a monster thmodel from my 24kWh eNV200 thmodels

eNV24kWh <- combine_thmodels(
  name = "eNV24kWh",
  thmodels = list(
    eNV200ac24kWh_2021,
    eNV200ac24kWh_2022,
    eNV200ac24kWh_2023,
    eNV200ac24kWh_2024,
    eNV200ac24kWh_2025mdy,
    eNV200ac24kWh_2025
  ))

usethis::use_data(eNV24kWh, overwrite = TRUE)
