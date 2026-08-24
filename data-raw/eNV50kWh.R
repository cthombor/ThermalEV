## create a monster thmodel from my 50kWh eNV200 thmodels

eNV50kWh <- combine_thmodels(
  name = "eNV50kWh",
  thmodels = list(eNV200noac50kWh, eNV200ac50kWh))

usethis::use_data(eNV50kWh, overwrite = TRUE)
