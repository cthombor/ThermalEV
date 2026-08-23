#' new_ocv_model: S3 class constructor
#'
#' @param name # user-friendly name, used for titles of plots
#' @param thmodels # provenance (a list of thmodels)
#'
#' @returns ocv_model, a named list
#' @export
#'
#' @examples
#' om <- new_ocv_model("eNV50kWh", list(eNV200ac50kWh, eNV200noac50kWh))
new_ocv_model <-
  function(
    name = "",
    thmodels = list()) {
  om <-
    list(
      name = "", # user-friendly name, used for titles of plots
      model = "", # either "LEAF" or "e-NV200"
      capacity = NULL, # nominal capacity, in kWh
      created.time = now(), # an untidy name, as in xts
      modified.last.time = now(),
      parameters = list(), # resistance parameters e.g. packr85
      ocv_tbl = tibble(),
      # if ocv_tbl has any rows, then this must be a tabular estimate of the OCV
      # at 25 °C, as a function of SOC.
      logdata = tibble(),
      # if logdata has any rows, then this must be selected columns from logdata
      # in the listed thmodels: minimally SOC, pack_volts, pack_amps,
      # pack_avg_temp, hx, date_time.
      thmodels = list() # provenance (a list of thmodel filnms)
    )
  om <- structure(om, class = "ocv_model")
  om$name <- name
  om$thmodels <- thmodels
  om$created.time = now()
  om$modified.last.time = now()
  if (length(thmodels) > 0) {
    m <- thmodels[[1]]
    om$model <- m$model
    om$capacity <- m$capacity
    om$parameters <- list(
      effective_pack_resistance = m$parameters[["effective_pack_resistance"]],
      packr85 = m$parameters[["packr85"]],
      arrhenius_resistance = m$parameters[["arrhenius_resistance"]],
      heat_capacity = m$parameters[["heat_capacity"]]
      )
    om$ocv_tbl <- m$parameters$ocv_tbl
    om$logdata <- m$logdata |>
      select(soc, pack_volts, pack_amps, pack_avg_temp, hx, date_time) |>
      mutate(soc = soc / 1e6) # 0.0 to 1.0 scale

    for (i in 1 : length(thmodels)) {
      m <- thmodels[[i]]
      if ((om$model != m$model) ||
          (om$capacity != m$capacity)) {
        warning("Incompatible data")
      }
      mld <- m$logdata |>
        select(soc, pack_volts, pack_amps, pack_avg_temp, hx, date_time) |>
        mutate(soc = soc / 1e6) # 0.0 to 1.0 scale
      om$logdata <- om$logdata |> rbind(mld) # quadratic runtime, ouch
      # Could be hack-optimised e.g. with pre-allocated lists, but that's more
      # trouble than it's worth.  Welcome to the second hell of the R inferno!
    }
  }

  om <- predict_volts(om) # initial voltage predictions

  return(om)
}
