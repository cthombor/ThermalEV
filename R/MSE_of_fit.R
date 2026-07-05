#' MSE_of_fit: compute the mean squared error of the estimation in a logtibble
#'
#' @param m, a thmodel
#'
#' @returns double
#' @export
#'
#' @examples
#' MSE_of_fit(predict_temp())
MSE_of_fit <- function(m) {
  t1 <- mutate(m$logdata,
               pred_error = pack_avg_temp - pred_pack_avg_temp,
               .keep = "used")
  t1 <- mutate(t1, pred_errorsq = pred_error * pred_error)
  t2 <- summarise(t1, mean = mean(pred_errorsq, na.rm = TRUE))
  return(t2$mean)
}
