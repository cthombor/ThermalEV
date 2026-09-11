#' fit_r_to_ocv: uses optim() to find a best-fit pack resistance to an ocv_model
#' @param om an ocv_model
#' @param iter_count controls convergence on predicted temps
#' @param trace 0 for silent, 1 for minimal, 2 for verbose
#'
#' @returns modified ocv_model, with best-fit resistive params
#' @export
#'
#' @examples
#'om <- fit_r_to_ocv(new_ocv_model("eNV50kWh", list(eNV200ac50kWh, eNV200noac50kWh)))
fit_r_to_ocv <- function(
    om = new_ocv_model(),
    iter_count = 4,
    trace = 1
    ) {

  stopifnot(length(om$logdata$soc) > 0)

#' fom: local fcn, for use by optim()
#'
#' Side effect: updates ocv_model om in the calling environment
#'
#' @param x parameter list
#'
  fom <- function(x = c(packr, arr, packr85)) {
    om <- predict_volts(
      om,
      effective_pack_resistance = x[1],
      arrhenius_resistance = x[2],
      packr85 = x[3]
    )
    return(MSE_of_ocv_fit(om))
  }

  start_packr <- om$parameters[["effective_pack_resistance"]]
  start_arr = om$parameters[["arrhenius_resistance"]]
  start_packr85 = om$parameters[["packr85"]]
  start_hc = om$parameters[["heat_capacity"]]
  start_mean_r <- mean(om$logdata$eff_packr, na.rm = TRUE)
  # fixme: start optimisation from values specified in call to fit_ocv()

  bestfit <- optim(
    par = c(start_packr, start_arr, start_packr85),
    fn = fom,
    lower = c(max(10, start_packr - 100), # packr must be resistive
              start_arr - 1000,
              max(10, start_packr85 - 50)), # packr85 must be resistive
    upper = c(start_packr + 100,
              start_arr + 1000,
              start_packr85 + 300),
    control = list(maxit = iter_count,
                   ndeps = c(1, 50, 1)), # initial size of steps
    method = "L-BFGS-B")

  best_packr <- bestfit$par[1]
  best_arr <- bestfit$par[2]
  best_packr85 <- bestfit$par[3]

  om$parameters[["effective_pack_resistance"]] <- best_packr
  om$parameters[["arrhenius_resistance"]] <- best_arr
  om$parameters[["packr85"]] <- best_packr85

  # evaluate predict_ocv() on the best_fit parameters
  om <- predict_volts(
      om,
      effective_pack_resistance = best_packr,
      arrhenius_resistance = best_arr,
      packr85 = best_packr85
      )
  cat("MSE of fit:", MSE_of_ocv_fit(om), "\nWorst: \n")
  worsti <- which.max(om$logdata$pack_volts - om$logdata$pred_pack_volts)
  options(pillar.sigfig = 4)
  print(slice(om$logdata, worsti), width = Inf, max_footer_lines = 0)

  mean_r <- mean(om$logdata$eff_packr, na.rm = TRUE)

  new_hc <- start_hc * mean_r / start_mean_r
  # possibly: adjust the hc param with a best-guess, to speed optimisation
  # in a subsequent fit_model()
  # om$parameters[["heat_capacity"]] <- new_hc
  cat("Mean packr = ", mean_r, "; estimated heat capacity = ", new_hc, "\n")

  return(om)
}
