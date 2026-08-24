#' Concatenates thmodels.  Warning: quadratic runtime for long lists.
#'
#' @param name # user-friendly name, used for titles of plots
#' @param thmodels # a list of thmodels to be combined
#'
#' @returns thmodel
#' @export
#'
#' @examples
#' tm <- combine_thmodels("eNV50kWh", list(eNV200ac50kWh, eNV200noac50kWh))
combine_thmodels <-
  function(
    name = "",
    thmodels = list()) {
  tm <- new_thmodel()
  if (length(thmodels) > 0) {
    m <- thmodels[[1]]
    if (name != "") {
      tm$name <- name
    } else {
      tm$name <- paste0("(", m$name, ", ...)")
    }
    tm$model <- m$model
    tm$capacity <- m$capacity
    tm$parameters <- m$parameters
    tm$ocv_tbl <- m$ocv_tbl
    tm$logdata <- m$logdata
    if (!("rpm" %in% names(tm$logdata))) {
      tm$logdata <- tm$logdata |> mutate(
        rpm = NA # older LeafSpy logs lack this column
      )
    }

    for (i in 2 : length(thmodels)) {
      m <- thmodels[[i]]
      if ((tm$model != m$model) ||
          (tm$capacity != m$capacity)) {
        warning("Incompatible data")
      }
      if (!("rpm" %in% names(m$logdata))) {
        m$logdata <- m$logdata |> mutate(
          rpm = NA
        )
      }
      tm$logdata <- tm$logdata |> rbind(m$logdata) # quadratic runtime, ouch
      # Could be hack-optimised e.g. with pre-allocated lists, but that's more
      # trouble than it's worth.  Welcome to the second hell of the R inferno!
    }
  }

  tm <- predict_temp(tm) # initial thermal predictions

  return(tm)
}
