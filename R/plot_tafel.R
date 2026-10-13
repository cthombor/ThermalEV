#' plot_tafel: plot an evolution of cathodic overpotential
#'
#' @param rho, ratio of i₀ to i, with i == 1 mA/cm²
#' @param ctafel, effective capacitance in μF/cm²
#' @param acfrt, cathodic transfer rate coefficient at 25 °C
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_tafel()
plot_tafel <- function(rho = 0.5,
                       ctafel = 50,
                       acfrt = 2
)
{
  i <- 1 # a nominal current density, in mA/cm²
  t <- c(0:100) # timesteps (in s)
  eta <- (1 / acfrt) *
    log(rho + (1 - rho) * exp(-acfrt * i * t / ctafel))
  plot(t, eta)
}
