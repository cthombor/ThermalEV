#' Plot cumsum of heats (Joule etc)
#'
#' @param m a thmodel, with predictions
#' @param from_date starting date/time
#' @param to_date ending date/time
#' @param from_idx starting index in thmodel, ignored if !is.null(from_date)
#' @param to_idx ending index in thmodel, ignored if !is.null(to_date)
#'
#' @returns an Environment
#' @export
#'
#' @examples
#' plot_fit_heats(eNV200noac50kWh, from_idx = 1, to_idx = 10)
plot_fit_heats <- function(m,
                     from_date = NULL,
                     to_date = NULL,
                     from_idx = NULL,
                     to_idx = NULL
)
{
  plotdata <- m$logdata |> arrange(date_time)
  # curiously, xts insists on UTC for stored dates & times
  if (!is.null(from_date)) {
      from_date <- as.POSIXct(from_date, tz = "UTC")
  }
  if (!is.null(to_date)) {
    to_date <- as.POSIXct(to_date, tz = "UTC")
  }
  from_idx <- ifelse(is.null(from_date),
                     ifelse(is.null(from_idx), 1, from_idx),
                     dplyr::first(which(plotdata$date_time >= from_date)))
  to_idx <- ifelse(is.null(to_date),
                   ifelse(is.null(to_idx), nrow(m$logdata), to_idx),
                   dplyr::last(which(plotdata$date_time <= to_date)))
  if (is.na(from_idx) || is.na(to_idx)) {
    warning("Date out of range")
  } else if (to_idx < from_idx) {
    warning("No data in this range")
  } else {
    pd <- plotdata |>
      slice(from_idx:to_idx) |>
      mutate(
        'Joule heat' = cumsum(replace_na(pred_Joule_heating, 0)) /
          3.6e6, #in kWh
        'Entropic heat' = cumsum(replace_na(pred_entropic_heating_rev, 0)) /
          3.6e6,
        'Polarisation heat' = cumsum(replace_na(pred_polarisation_heating, 0)) /
          3.6e6,
        'Heat pump cooling' = - cumsum(replace_na(heat_pump_cooling, 0)) /
          3.6e6
      )

    tjh <- last(pd$'Joule heat')
    teh <- last(pd$'Entropic heat')
    tph <- last(pd$'Polarisation heat')
    thpc <- last(pd$'Heat pump cooling')
    th <- tjh + teh + tph
    cat("Total Joule heat =",
        format(last(pd$'Joule heat'), digits = 3),
        "kWh (", round(tjh / th  * 100, 1), "%)\n")
    cat("Total entropic heat =",
        format(last(pd$'Entropic heat'), digits = 3),
        "kWh (", round(teh / th  * 100, 1), "%)\n")
    cat("Total polarisation heat =",
        format(last(pd$'Polarisation heat'), digits = 3),
        "kWh (", round(tph / th  * 100, 1), "%)\n")
    cat("Total heat pump cooling =",
        format(last(pd$'Heat pump cooling'), digits = 3),
        "kWh (", round(thpc / th  * 100, 1), "%)\n")

    pd <- pd |>
      select(date_time,
             'Joule heat',
             'Entropic heat',
             'Polarisation heat',
             'Heat pump cooling') |>
      xts::as.xts()

    title = paste0(
      m$name,
      ": r = ",
      format(m$parameters$effective_pack_resistance, digits = 3),
      # n.b. U+2009 is a thin space
      ifelse(
        m$parameters$packr85 !=
          m$parameters$effective_pack_resistance,
        paste0(
          "\u2009mΩ, packr85 = ",
          format(m$parameters$packr85, digits = 3),
          collapse = ""
        ),
        ""
      ),
      "\u2009mΩ, pi = ",
      format(m$parameters$polarisation_irr, digits = 3),
      "\u2009mΩ, ts = ",
      format(m$parameters$tafel_slope, digits = 3),
      ", λp = ",
      format(m$parameters$lambda_polarisation, digits = 3),
      "\u2009s, eh = ",
      format(m$parameters$entropic_heat, digits = 3),
      "\u2009kJ/V, λt = ",
      format(m$parameters$lambda_polarisation, digits = 3),
      "\u2009s, λp = ",
      format(m$parameters$lambda_module_to_ambient, digits = 3),
      # passive
      "\u2009h, λa = ",
      format(m$parameters$lambda_module_AC_to_ambient, digits = 3),
      # A/C
      "\u2009h,\n    fanp = ",
      format(m$parameters$fan_power, digits = 3),
      "\u2009W, COP = ",
      format(m$parameters$COP, digits = 3)
    )

    pd |>
      plot(
        legend.loc = "topleft",
        type = "p",
        pch = 1,
        main.timespan = FALSE,
        format.labels = "%Y-%m-%d %H:%M",
        main = title)
  }

}
