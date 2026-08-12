# new_ocv_model: S3 class constructor
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
      created.time = now(), # an untidy varname, as in xts
      modified.last.time = now(),
      parameters = list(),
      # if length(m$parameters) > 0, these are secondary parameters in
      # the non-linear optimisation which estimated ocv_tbl from logdata.
      logdata = tibble(),
      # if logdata has any rows, then this must be selected columns from logdata
      # in the listed thmodels: minimally SOC, pack_volts, pack_amps,
      # pack_avg_temp, date_time.
      ocv_tbl = tibble(),
      # if ocv_tbl has any rows, then this must be a tabular estimate of the OCV
      # at 25 °C, as a function of SOC.
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
    om$logdata <- m$logdata |>
      select(soc, pack_volts, pack_amps, pack_avg_temp, date_time)
  }
  # fixme: add data from other thmodels
  # fixme: add an initial call to fit_ocv()
  return(om)
}
