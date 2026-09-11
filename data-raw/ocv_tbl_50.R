## preserve ocv_tbl from om_eNV50kWh (after fitting this ocv_model)

ocv_tbl_50 <- om_eNV50kWh$ocv_tbl

usethis::use_data(ocv_tbl_50, overwrite = TRUE)
