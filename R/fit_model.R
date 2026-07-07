#' fit_model: uses nlm() to find a best-fit, with MSE criterion
#'
#' @param m a thmodel
#'
#' @param effective_pack_resistance in mOhms
#' @param lambda_cell_to_pack in seconds
#' @param lambda_pack_to_ambient in hours
#' @param lambda_pack_AC_to_ambient in hours
#' @param fan_power in W
#' @param COP coefficient of heatpump performance, dimensionless
#' @param fixed_parameters length-7 Boolean vector
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
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
                      fixed_parameters = c(F, F, F, F, T, T, T),
                      from_date = NULL,
                      to_date = NULL,
                      from_idx = NULL,
                      to_idx = NULL) {

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
#' fm(c(500, 120, 8, 2, 300, 3))
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

  # all logs "should" be sorted on date-time... but just in case...
  plotdata <- m$logdata |> arrange(date_time)
  # curiously, xts insists on UTC for stored dates & times
  from_idx <- ifelse(is.null(from_date),
                     ifelse(is.null(from_idx), 1, from_idx),
                     dplyr::first(which(
                       plotdata$date_time >= as.POSIXct(from_date, tz = "UTC")
                     )))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                   dplyr::last(which(
                     plotdata$date_time <= as.POSIXct(to_date, tz = "UTC")
                   )))
  if (is.na(from_idx) || is.na(to_idx)) {
    warning("Date out of range")
  } else if (from_idx >= to_idx) {
    warning("from_date is not before to_date")
  }

  origmodel <- m
  m$logdata <- m$logdata |> slice(from_idx:to_idx)

  # n.b. the box-constrained optimisation of L-BFGS-B throws an error if any
  # dimension of the box is zero, so we add an epsilon and hope for the best
  bestfit <- optim(
    par = c(packr, lambda1, lambda2, lambda3, fanp, COP),
    fm,
    lower = c(if (fixed_parameters[1]) packr else 100,
              if (fixed_parameters[2]) lambda1 else 0,
              if (fixed_parameters[3]) lambda2 else 0,
              if (fixed_parameters[4]) lambda3 else 0,
              if (fixed_parameters[5]) fanp else 0,
              if (fixed_parameters[6]) COP else 0),
    upper = c(if (fixed_parameters[1]) packr + 0.01 else 1200,
              if (fixed_parameters[2]) lambda1 + 0.001 else 100,
              if (fixed_parameters[3]) lambda2 + 0.0001 else 10,
              if (fixed_parameters[4]) lambda3 + 0.0001 else 10,
              if (fixed_parameters[5]) fanp + 0.01 else 500,
              if (fixed_parameters[6]) COP + 0.0001 else 5),
    control = list(maxit = 2),
    method = "L-BFGS-B")

  # remove epsilons from the best-fit of fixed parameters
  best_packr = if (fixed_parameters[1]) packr else bestfit$par[1]
  best_lambda1 = if (fixed_parameters[2]) lambda1 else bestfit$par[2]
  best_lambda2 = if (fixed_parameters[3]) lambda1 else bestfit$par[3]
  best_lambda3 = if (fixed_parameters[4]) lambda1 else bestfit$par[4]
  best_fanp = if (fixed_parameters[5]) fanp else bestfit$par[5]
  best_COP = if (fixed_parameters[6]) COP else bestfit$par[6]

  # evaluate predict_temp() on the best_fit parameters
  m <- predict_temp(
    m,
    effective_pack_resistance = best_packr,
    lambda_cell_to_pack = best_lambda1,
    lambda_pack_to_ambient = best_lambda2,
    lambda_pack_AC_to_ambient = best_lambda3,
    fan_power = best_fanp,
    COP = best_COP
  )
  if (abs(bestfit$value - MSE_of_fit(m)) > 0.05) {
    warning(c("Fixed best-fit parameters are highly sensitive.",
            " Mean square error epsilon-differential: (",
            MSE_of_fit(m),
            ",", bestfit$value, ")"))
    #n.b. we assume that optim() never returns a value outside the limiting box
  }
  if ((to_idx - from_idx + 1) < length(origmodel$logdata$err_pred)) {
    cat("MSE of fit over the specified range:", bestfit$value, "\n")
  }

  origmodel <- predict_temp(
    origmodel,
    effective_pack_resistance = best_packr,
    lambda_cell_to_pack = best_lambda1,
    lambda_pack_to_ambient = best_lambda2,
    lambda_pack_AC_to_ambient = best_lambda3,
    fan_power = best_fanp,
    COP = best_COP
  )
  cat("MSE of fit over the full dataset:", MSE_of_fit(origmodel), "\n")

  return(origmodel)
}
