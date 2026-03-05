#' fit_model, runs nlm() on predict_temp to minimise Rsq.
#'

library(tidyverse)
library(xts)

#' @param pack_r
#'
#' @param m
#' @param packr
#' @param lambda1
#' @param lambda2
#' @param print.level
#'
#' @returns list from nlm(), describing its best-fit
#' @export
#'
#' @examples
#' m <- fit_model(print.level = 2)

fit_model <- function(m = NULL,
                      packr = 4,
                      lambda1 = 60,
                      lambda2 = 10000,
                      print.level = 1) {

  fm <- function(x = c(packr, lambda1, lambda2)) {
    m <- predict_temp(
      m,
      effective_pack_resistance = x[1],
      lambda_cell_to_pack = x[2],
      lambda_pack_to_ambient = x[3]
    )
    return(MSE_of_fit(m))
  }

  if (is.null(m)) m <- munge_logfile()  # use our default logfile

  m$fit <- nlm(fm,
               p = c(packr, lambda1, lambda2),
               print.level = print.level)

  # evaluate predict_temp() one last time, on the best fit
  m <- predict_temp(
    m,
    effective_pack_resistance = m$fit$estimate[1],
    lambda_cell_to_pack = m$fit$estimate[2],
    lambda_pack_to_ambient = m$fit$estimate[3]
  )

  return(m)
}

#' MSE_of_fit: compute the mean squared error of the estimation in a logtibble
#'
#' @param m, a thmodel
#'
#' @returns double
#' @export
#'
#' @examples
MSE_of_fit <- function(m) {
  t1 <- mutate(m$logdata,
               pred_error = pack_avg_temp - pred_pack_avg_temp,
               .keep = "used")
  t1 <- mutate(t1, pred_errorsq = pred_error * pred_error)
  t2 <- summarise(t1, mean = mean(pred_errorsq, na.rm = TRUE))
  return(t2$mean)
}
