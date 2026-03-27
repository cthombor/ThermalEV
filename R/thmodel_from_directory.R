#' Utility to import an entire directory of csv files into a thmodel
#'
#' Warning: any update to LeafSpy may cause it to produce logfiles with a
#' different set of column names, so some additional data-tidying may be
#' required.
#'
#' warning: this script munges the csv files in the target directory, using
#' 'munge_logfile()'
#'
#' @param logfildir path to directory containing csv files
#' @param USonian_dates TRUE for mdy, FALSE for dmy
#' @param name friendly name for this thmodel
#' @param capacity nominal capacity of the pack, in kWh
#'
#' @returns a thmodel
#' @export
#'
#' @examples
#' m <- thmodel_from_directory()

thmodel_from_directory <- function(logfildir = "data-raw/eNV200noac50kWh",
                                   name = NULL,
                                   USonian_dates = FALSE,
                                   capacity = NULL) {

  filnm_list <- list.files(here::here(logfildir))

  m_list <- vector("list", length = length(filnm_list))
  for (i in seq(length(filnm_list))) {
    x <- filnm_list[[i]]
    print(paste(i, x, "..."))
    m_list[[i]] <- munge_logfile(logfildir = logfildir,
                  logfilnm = x,
                  logname = x,
                  USonian_dates = USonian_dates)
  }
  tbl_list <- lapply(m_list, function(x) x$logdata)

  t <- dplyr::bind_rows(tbl_list)

  cname <- janitor::make_clean_names(logfildir, case = "none")

  m <- new_thmodel()
  m$logdata <- arrange(t, date_time)
  m$filnm <- ""
  m$name <- ifelse(is.null(name), cname, name)
  m$capacity <- capacity
  m$fildir <- logfildir
  return(m)
}


