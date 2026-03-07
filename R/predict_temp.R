#' Uses a 3-parameter thermal model to predict temperatures in a LeafSpy log
#'
#' A LeafSpy logfile may be specified to this function by filename and
#' directory, in which case this logfile is munged -- to mitigate the privacy
#' risk of publishing a VIN, and to revise column names so that they're tidy. In
#' normal use, the logfile is specified to predict_temp() by a thmodel object
#' -- which has metadata describing its provenance and the values of the
#' modelling parameters, and which has a tibble-translation of the orginal
#' LeafSpy csv with additional columns for the predictions of the model and for
#' convenience when plotting.
#'
#' Notes on heat capacity:
#'
#' The 96 cells in my aftermarket 50kWh pack weigh 2.13 kg apiece, so there's a
#' total of roughly 200kg of water (at 4.13 J/gK) in the electrolyte.  There'll
#' be some additional heat content in the 100kg of non-cell contents in the
#' pack. Polyethylene is 2.0 J/gK, steel is 0.5 J/gK, everything else is less.
#' As a round number, the heat content of the pack is thus 1.0e6.  This is a
#' secondary parameter in our modelling because the Joule heating (in K) of a
#' pack is the square of its amperage, multiplied by its effective resistance
#' and divided by its heat capacity (in J/K).
#'
#' @param tmodel a thmodel, optional
#' @param effective_pack_resistance in mOhms, a primary parameter
#' @param lambda_cell_to_pack in seconds, a primary parameter
#' @param lambda_pack_to_ambient in hours, a primary parameter
#' @param heat_capacity in J/K, a secondary parameter
#' @param min_segment_length shorter sequences of samples are ignored
#' @param logfilnm name of a csv logfile to be read, if is.null(tmodel)
#' @param logfildir directory in which the logfile is located
#'
#' @returns a thmodel
#' @export
#'
#' @examples
#' m <- predict_temp() # uses data-raw/log26Jan26.csv
#' m <- m |> predict_temp(effective_pack_resistance = 0.5)

predict_temp <- function(tmodel = NULL,
                         effective_pack_resistance = NA,
                         lambda_cell_to_pack = NA,
                         lambda_pack_to_ambient = NA,
                         heat_capacity = NA,
                         min_segment_length = 50,
                         logfilnm = "log26Jan2026.csv",
                         logfildir = "data-raw") {

  if (!nzchar(logfilnm) && is.null(tmodel)) {
    stop("Aborting. Please specify the name of a LeafSpy logfile.")
  }
  else {
    m <- tmodel
    if (is.null(m) || m$name == "") {
      m <- munge_logfile(logfilnm = logfilnm, logfildir = logfildir)
    }
  } # endif (!nzchar(logfile))

  if (length(m$parameters) == 0) {
    m <- default_params(m)
  }
  logtibble <- m$logdata

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
  if (!is.na(heat_capacity)) {
    m$parameters[["heat_capacity"]] <- heat_capacity
  }

  # read a full set of params
  effective_pack_resistance <- m$parameters[["effective_pack_resistance"]]
  lambda_cell_to_pack <- m$parameters[["lambda_cell_to_pack"]]
  lambda_pack_to_ambient <- m$parameters[["lambda_pack_to_ambient"]]
  heat_capacity <- m$parameters[["heat_capacity"]]

  # n.b. additional secondary parameters e.g. ones used only by nlm(),
  # may be stored in m$parameters

  if (!"delta_t" %in% names(logtibble)) {
    # a minor optimisation: we avoid recomputing these columns

    #compute delta_t for runs of near-consecutive samples
    logtibble <- logtibble |>
      mutate(delta_t = date_time - lag(date_time))

    sampling_interval <- as.double(median(logtibble$delta_t, na.rm = TRUE))
    # multiple missing samples will terminate a predictive segment
    # n.b. isolated missing samples do not hugely affect our model's predictions
    # time-stamps in the logs have a precision of 1 second
    max_delta_t <- 2 * sampling_interval + 2
    logtibble <- logtibble |>
      mutate(delta_t = ifelse(delta_t > max_delta_t, NA, delta_t))

    # strangely, pack_t3_c is uniformly NA in all my logfiles.
    logtibble <- logtibble |>
      mutate(pack_avg_temp = rowMeans(across(c(
        pack_t1_c, pack_t2_c, pack_t4_c
      ))), .before = cp1)

    # n.b. csv file corruption may result in a temperature in Fahrenheit
    # appearing as a wildly-improbable value in a Centigrade column
    outlier_temps <- which(logtibble$pack_t4_c > 50)
    if (length(outlier_temps) > 0) {
      warning(paste(length(outlier_temps) > 0),
              "temperatures greater than 50 in the pack_t4_c column")
    }

    #todo: ? mark outliers with NA in delta_t, so they'll be ignored.
    # downsides: the csv file might be repairable, an occasional
    # outlier shouldn't greatly affect the estimates from nlm(), and
    # any automagic outlier-rejection is hazardous

    # rate of heat gain (in K/s)
    logtibble <- logtibble |>
      mutate(delta_K_delta_t =
               (pack_avg_temp - lag(pack_avg_temp)) / delta_t,
             .before = cp1)

    #munge soc to a percentage, for ease of plotting and by convention
    logtibble <- logtibble |> mutate (psoc = soc / 10000, .before = cp1)

  }

  # we now predict temperatures, using the parameters

  # predicted Joule heating of cells (in W)
  # n.b. the resistance is in mOhms
  logtibble <- logtibble |>
    mutate(
      pred_Joule_heating =
        pack_amps * pack_amps *
        effective_pack_resistance / 1000,
      .before = cp1
    )

  # we now recompute the sampling_interval with more precision, by using the
  # mean rather than the median as we had done when defining segments in the
  # logfile pre-processing step. This calculation is insensitive to the long
  # intervals between segments, and is likely is at a higher precision than the
  # 1-second resolution of timestamps as recorded in the logfile)
  sampling_interval <- as.double(mean(logtibble$delta_t, na.rm = TRUE))

  # unlagged predicted per-sample delta-heating of pack (in temperature K),
  # based on our roughly-estimated heat_capacity. If this parameter is
  # modified, then the best-fit value of effective_pack_resistance is affected
  # (precisely in inverse proportion), since it is the ratio between these two
  # parameters which is the constant of proportionality between the square of
  # pack amperage and its "Joule heating" in units of K/s.
  logtibble <- logtibble |>
    mutate(
      pred_heating_unlagged =
        pred_Joule_heating / heat_capacity * sampling_interval,
      .before = cp1
    )

  # we use an Exponential Moving Average filter on a per-sample basis: rather
  # inaccurate when there are missing samples; but can be efficiently computed
  # on any modern (deeply-pipelined) CPU.  Furthermore, it is undistorted by the
  # roundoff errors in the time-stamps (at 1-second precision!) on the samples
  EMA_parameter_cell_to_pack <- sampling_interval / lambda_cell_to_pack

  # we now lurch from the tidyverse into the wilds of xts. The EMA is a simple
  # recursive filter; but R's runtime is hostile to recursion.  It's possible to
  # use sapply() to implement a reasonably-efficient tail-recursion, but its
  # correctness relies on an undocumented expectation of sapply: that it
  # performs a sequential, in-order execution of its FUN, and not a vectorised
  # implementation (which would read all of its inputs fully, rather than
  # stalling the computation of the second element of its output until the
  # computation of its first element has completed) See stackoverflow.com/
  # questions/49348870/tibbletime-previous-days-close/49373709#
  # comment140995212_49373709 and github.com/tidyverse/dbplyr/issues/1108

  # predicted delta-heating of pack (in K), with exponential lag, grouped
  # by the gaps in the sampling
  w <- which(is.na(logtibble$delta_t))
  nsegments <- length(w)
  wstart <- w
  wend <- lead(w) - 1
  wend[nsegments] <- length(logtibble$delta_t)
  wexclude <- (wend - wstart) < min_segment_length

  unlagged_heat <- as.xts(logtibble$pred_heating_unlagged, logtibble$date_time)
  lagged_heat <- as.xts(vector(mode = "double",
                               length = length(unlagged_heat)),
                        logtibble$date_time)

  for (i in seq(nsegments)[which(!wexclude)]) {
    lagged_heat[wstart[i]:wend[i]] <-
      stats::filter(
        unlagged_heat[wstart[i]:wend[i]] * EMA_parameter_cell_to_pack,
        1. - EMA_parameter_cell_to_pack,
        method = "recursive",
        init = 0.
      )
  }

  pack_avg_temp_xts <-
    xts(logtibble$pack_avg_temp, logtibble$date_time)

  # predict pack temps from cumsum of lagged delta-heat in each segment
  segment_starts <- logtibble$date_time[wstart]
  pred_pack_avg_temp_xts <- lag(lagged_heat)

  pred_pack_avg_temp_xts[wstart] <- pack_avg_temp_xts[wstart] # observations
  for (i in seq(nsegments)[which(!wexclude)]) {
    pred_pack_avg_temp_xts[wstart[i]:wend[i]] <-
      cumsum(pred_pack_avg_temp_xts[wstart[i]:wend[i]]) # predictions
  }

  # we now revert to base R, in order to apply a recursive filter which is
  # outside the scope of stats::filter().
  #
  # This filter also has an exponential decay.  It is a first-order model of the
  # cooling of the pack by its convection to the ambient air (and also by some
  # conduction to the vehicle's frame -- which is assumed to be in thermal
  # equilibrium with the ambient temperature as measured by a thermosensor at
  # the front of the vehicle).
  #
  # TODO: adjust the time constant so that it decreases with the vehicle's
  # velocity.  Possibly it'll be adequate to multiply the time constant by
  # the vehicle's velocity divided by a fitted parameter (200 km/h, initially)

  # The computational kernel of this filter could be executed quite efficiently
  # on any superscalar CPU.  To gain this efficiency, it'd be necessary to
  # express it in a language (C, Fortran, Java, ...) which offers an optimising
  # compiler. This filter could not be calculated efficiently on any vector
  # supercomputer or on any GPU, because the updated prediction for the previous
  # timestep is an input to the prediction of the current timestep. It might be
  # interesting to compare per-element runtimes of this unvectorisable filter in
  # various implementations of various interpreted languages (R, Python,
  # unjitted Java, ...), on various CPUs.
  ambient_v <- logtibble$ambient
  pred_temp_v <- as.vector(pred_pack_avg_temp_xts[,1,drop=TRUE])
  old_pred_v <- pred_temp_v

  # n.b. the second time constant is in hours
  EMA_parameter_pack_to_ambient <-
    sampling_interval / (lambda_pack_to_ambient * 3600)
  for (i in seq(nsegments)[which(!wexclude)]) {
    # It might be interesting to benchmark R's Tailcall() against the following
    # "manual" TCO of a simple recursive filter.  See
    # stackoverflow.com/questions/78979492/optimization-of-tail-recursion-in-r
    prev_newpred <- pred_temp_v[wstart[i]] # base case
    prev_oldpred <- prev_newpred
    for (j in seq(wstart[i] + 1, wend[i])) {
      curr_oldpred <- old_pred_v[j]
      # new delta-temp is the old delta-temp plus a small shift toward ambient
      curr_newpred <- prev_newpred +
        (curr_oldpred - prev_oldpred) +
        (ambient_v[j] - prev_newpred) * EMA_parameter_pack_to_ambient
      pred_temp_v[j] <- curr_newpred
      prev_oldpred <- curr_oldpred
      prev_newpred <- curr_newpred
    }
  }

  pred_temp_v[wstart] <- NA # these are not predictions

  # return to the tidyverse!  Hooray!!
  logtibble <- logtibble |>
    mutate(pred_pack_avg_temp = pred_temp_v,
           .before = "cp1") |>
    select(!c(pred_Joule_heating, pred_heating_unlagged))
  m$logdata <- logtibble
  m$modified.last.time <- now()
  return(m)

}
