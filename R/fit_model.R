#' fit_model: uses nlm() to find a best-fit, with MSE criterion
#'
#' @param m a thmodel
#'
#' @param print.level 0 for quiet, 1 for start and endpoints, 2 for verbose
#' @param effective_pack_resistance in mOhms
#' @param lambda_cell_to_pack in seconds
#' @param lambda_pack_to_ambient in hours
#' @param lambda_pack_AC_to_ambient in hours
#' @param fan_power in W
#' @param COP coefficient of heatpump performance, dimensionless
#'
#' @returns a list, retval from nlm() describing its best-fit
#' @export
#'
#' @examples
#' m <- fit_model(print.level = 2)
fit_model <- function(m = NULL,
                      effective_pack_resistance = NA,
                      lambda_cell_to_pack = NA,
                      lambda_pack_to_ambient = NA,
                      lambda_pack_AC_to_ambient = NA,
                      fan_power = NA,
                      COP = NA,
                      print.level = 1) {

#' fm: interface to predict_temp(), for use by nlm()
#'
#' Side effect: updates thmodel `m` in the calling environment
#'
#' @param x parameter list
#'
#' @returns MSE of the fit
#' @export
#'
#' @examples
#' fm(c(0.4,120,8,2,3))
  fm <- function(x = c(packr, lambda1, lambda2, lambda3, fanp, COP)) {
    m <- predict_temp(
      m,
      effective_pack_resistance = x[1],
      lambda_cell_to_pack = x[2],
      lambda_pack_to_ambient = x[3],
      lambda_pack_AC_to_ambient = x[4],
      fan_power = x[5],
      COP = x[6]
    )
    return(MSE_of_fit(m))
  }

  if (is.null(m)) m <- munge_logfile()  # use our default logfile

  stopifnot(length(m$parameters == 0) || length(m$parameters == 7))
  if (length(m$parameters) == 0) {
    m <- default_params(m)
  }

  # param values specified in the method call have precedence. Side effect:
  # if m$parameters is malformed, throw a "subscript out of bounds" error
  if (!is.na(effective_pack_resistance)) {
    m$parameters[["effective_pack_resistance"]] <- effective_pack_resistance
  }
  if (!is.na(lambda_cell_to_pack)) {
    m$parameters[["lambda_cell_to_pack"]] <- lambda_cell_to_pack
  }
  if (!is.na(lambda_pack_to_ambient)) {
    m$parameters[["lambda_pack_to_ambient"]] <- lambda_pack_to_ambient
  }
  if (!is.na(lambda_pack_AC_to_ambient)) {
    m$parameters[["lambda_pack_AC_to_ambient"]] <- lambda_pack_AC_to_ambient
  }
  if (!is.na(fan_power)) {
    m$parameters[["lambda_cooling_power"]] <- fan_power
  }
  if (!is.na(COP)) {
    m$parameters[["COP"]] <- COP
  }

  # read a full set of primary factors into shorthand vars
  packr <- m$parameters[["effective_pack_resistance"]]
  lambda1 <- m$parameters[["lambda_cell_to_pack"]]
  lambda2 <- m$parameters[["lambda_pack_to_ambient"]]
  lambda3 <- m$parameters[["lambda_pack_AC_to_ambient"]]
  fanp <- m$parameters[["fan_power"]]
  COP <- m$parameters[["COP"]]

  m$fit <- nlm(fm,
               p = c(packr, lambda1, lambda2, lambda3, fanp, COP),
               print.level = print.level)

  # evaluate predict_temp() one last time, on the best fit
  m <- predict_temp(
    m,
    effective_pack_resistance = m$fit$estimate[1],
    lambda_cell_to_pack = m$fit$estimate[2],
    lambda_pack_to_ambient = m$fit$estimate[3],
    lambda_pack_AC_to_ambient = m$fit$estimate[4],
    fan_power = m$fit$estimate[5],
    COP = m$fit$estimate[6]
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
#' MSE_of_fit(predict_temp())
MSE_of_fit <- function(m) {
  t1 <- mutate(m$logdata,
               pred_error = pack_avg_temp - pred_pack_avg_temp,
               .keep = "used")
  t1 <- mutate(t1, pred_errorsq = pred_error * pred_error)
  t2 <- summarise(t1, mean = mean(pred_errorsq, na.rm = TRUE))
  return(t2$mean)
}
