## concatenates csv files in data-raw/eNV200noac50kWh/ to create
## data/eNV200noac50kWh.rda

# warning: this script munges csv files in data-raw/eNV200noac50kWh/, deleting
# any "VIN" column to maintain the security-by-obscurity defense against
# malicious use of this identifier.

# warning: this script interprets the entries in the Date/Time column of
# the csv files as "%d/%m/%Y %H:%M:%S", munging these into timestamps in
# in POSIXct format ("%Y-%m-$d %H:%M:%S) in a date_time column.  This filewrite
# avoids the hazard of anyone using Excel to edit a csv file in data-raw/.
# See leafspy.com/wp-content/uploads/2024/04/LeafSpy-Help-1.5.0.pdf for
# a discussion of the precision-loss hazard when Excel writes a csv file.

# warning: this script renames the column-headers in csv files,
# so that they're fully acceptable as column names in the tidyverse.
# In particular, Date/Time is munged into date_time.

# warning: this script deletes any Debug columns in the csv files.  V1.5.0
# of LeafSpy for Android has two such columns.  Duplicate column names
# are second-class entities in the tidyverse.

# Run once, when adding this dataset to the package:
# usethis::use_data_raw("eNV200noac50kWh")
# Warning: this command overwrites any existing eNV200noac50kWh.R.

library(tidyverse)
library(usethis)

#' thmodel_from_directory
#'
#' This utility may be used on data-raw directories other than eNV200noac50kWh
#'
#' Warning: any update to LeafSpy may cause it to produce logfiles with a
#' different set of column names, so some additional data-tidying may be
#' required
#'
#' @param logfildir
#' @param name
#'
#' @returns
#' @export
#'
#' @examples
#' m <- thmodel_from_directory()

thmodel_from_directory <- function(logfildir = "data-raw/eNV200noac50kWh",
                                   name = "eNV200noac50kWh") {

  filnm_list <- list.files(here::here(logfildir))

  m_list <- lapply(filnm_list, function(x)
    munge_logfile(logfildir = logfildir,
                  logfilnm = x))

  tbl_list <- lapply(m_list, function(x) x$logdata)

  t <- dplyr::bind_rows(tbl_list)

  m <- new_thmodel()
  m$logdata <- t
  m$name <- name
  m$filnm <- ""
  m$fildir <- logfildir

  return(m)
}

eNV200noac50kWh <- thmodel_from_directory()

usethis::use_data(eNV200noac50kWh, overwrite = TRUE)


