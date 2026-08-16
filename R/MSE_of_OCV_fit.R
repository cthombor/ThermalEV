#' MSE_of_ocv_fit: compute the mean squared error of the estimation
#'
#' @param om, an ocv_model
#'
#' @returns double
#' @export
#'
#' @examples
#' MSE_of_ocv_fit(
#'    predict_ocv(new_ocv_model("eNV50kWh", list(eNV200ac50kWh)), 150, -3500)))
MSE_of_ocv_fit <- function(om) {
  t1 <- om$logdata |>
    mutate(pred_error = pack_volts - pred_pack_volts,
           pred_errorsq = pred_error * pred_error) |>
    summarise(mean = mean(pred_errorsq, na.rm = TRUE))
  return(t1$mean)
}
