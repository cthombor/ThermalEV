#' fit_model, runs nlm() on predict_temp to minimise Rsq.
#' Side effect: updates global_logtibble
#'

library(tidyverse)
library(xts)

#' @param pack_r
#'
#' @param lambda_to_pack
#' @param logtib
#' @param logfil
#' @param lambda_to_ambient
#'
#' @returns list from nlm()
#' @export
#'
#' @examples
fit_model <- function(logfil = "26Jan26",
                      pack_r = 4,
                      lambda_to_pack = 60,
                      lambda_to_ambient = 10000)
{
  fm <- function(x = c(pack_r, lambda_to_pack, lambda_to_ambient))
  {
    return(MSE_of_fit(
      predict_temp(
        effective_pack_resistance = x[1],
        lambda_cell_to_pack = x[2],
        lambda_pack_to_ambient = x[3]
      )
    ))
  }

  # prime the pump! initialise global_logtibble from logfil
  predict_temp(
    logfile = logfil,
    effective_pack_resistance = pack_r,
    lambda_cell_to_pack = lambda_to_pack,
    lambda_pack_to_ambient = lambda_to_ambient
  )
  fit <- nlm(
    fm,
    p = c(pack_r, lambda_to_pack, lambda_to_ambient),
    print.level = 1
  )
  #todo: confirm that nlm()'s last run of predict_temp() is its best fit, i.e.
  #that it's safe to rely on global_logtibble for details of this fit

  #todo: rewrite fit_model() to call predict_temp() using nlm()'s fitted
  #parameters, and incorporate this logtibble in a tidied-up result from nlm()

  global_fit <<- fit
  return(fit)
}

#' MSE_of_fit: compute the mean squared error of the estimation in a logtibble
#'
#' @param logt a tibble, describing a fitted pack-heat model
#'
#' @returns double
#' @export
#'
#' @examples
MSE_of_fit <- function(logt) {
  t1 <- mutate(logt,
               pred_error = pack_avg_temp - pred_pack_avg_temp,
               .keep = "used")
  t1 <- mutate(t1, pred_errorsq = pred_error * pred_error)
  t2 <- summarise(t1, mean = mean(pred_errorsq, na.rm = TRUE))
  return(t2$mean)
}
