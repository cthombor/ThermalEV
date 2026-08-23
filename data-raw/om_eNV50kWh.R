## ocv_model for 50kWh eNV200

om_eNV50kWh <- new_ocv_model("eNV50kWh",
                             list(eNV200ac50kWh, eNV200noac50kWh))

cat("MSE of OCV fit:", round(MSE_of_ocv_fit(om_eNV50kWh), 2), "\n")

usethis::use_data(om_eNV50kWh, overwrite = TRUE)
