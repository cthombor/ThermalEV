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
#' @param name friendly name for this thmodel
#'
#' @returns a thmodel
#' @export
#'
#' @examples
#' m <- thmodel_from_directory()

thmodel_from_directory <- function(logfildir = "data-raw/eNV200noac50kWh",
                                   name = NULL) {

  filnm_list <- list.files(here::here(logfildir))

  m_list <- lapply(filnm_list, function(x)
    munge_logfile(logfildir = logfildir,
                  logfilnm = x,
                  logname = x))

  tbl_list <- lapply(m_list, function(x) x$logdata)

  t <- dplyr::bind_rows(tbl_list)

  cname <- janitor::make_clean_names(logfildir, case = "none")

  m <- new_thmodel()
  m$logdata <- t
  m$filnm <- ""
  m$name <- ifelse(is.null(name), cname, name)
  m$fildir <- logfildir

  return(m)
}


