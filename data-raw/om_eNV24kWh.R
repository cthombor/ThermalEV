## ocv_model for 24kWh eNV200

om_eNV24kWh <-
  new_ocv_model(
    "eNV24kWh",
    list(eNV24kWh))

cat("MSE of OCV fit:", round(MSE_of_ocv_fit(om_eNV24kWh), 2), "\n")

usethis::use_data(om_eNV24kWh, overwrite = TRUE)
