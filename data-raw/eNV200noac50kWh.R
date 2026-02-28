## concatenate csv files in data-raw/eNV200noac50kWh/ to create
## data/eNV200noac50kWh.rda

# warning: this script modifies csv files in data-raw/eNV200noac50kWh/, deleting
# any "VIN" column to maintain the security-by-obscurity defense against
# malicious use of this identifier.

# warning: this script interprets the entries in the Date/Time column of
# the csv files as "%d/%m/%Y %H:%M:%S", rewriting these as timestamps in
# in POSIXct format ("%Y-%m-$d %H:%M:%S) in a date_time column.  This filewrite
# avoids the hazard of anyone using Excel to edit a csv file in data-raw/.
# See leafspy.com/wp-content/uploads/2024/04/LeafSpy-Help-1.5.0.pdf for
# a discussion of the precision-loss hazard when Excel writes a csv file.

# warning: this script renames the column-headers in csv files,
# so that they're fully acceptable as column names in the tidyverse.
# In particular, Date/Time is transmogrified into date_time.

# warning: this script deletes any Debug columns in the csv files.  V1.5.0
# of LeafSpy for Android has two such columns.  Duplicate column names
# are second-class entities in the tidyverse.

# Run once, when adding this dataset to the package:
# usethis::use_data_raw("eNV200noac50kWh")
# Warning: this command overwrites any existing eNV200noac50kWh.R.

library(tidyverse)
library(usethis)
library(xts)
library(janitor)

filnm_list = list.files(here::here("data-raw/eNV200noac50kWh/"))
tbl_list = vector("list", length(filnm_list))

for (i in seq(length(filnm_list))) {
  tbl <- read_csv(
    here::here("data-raw/eNV200noac50kWh", filnm_list[i]),
    n_max = 1,
    # read headers
    show_col_types = FALSE,
    name_repair = "unique_quiet"
  )
  if ("VIN" %in% names(tbl)) {
    # this csv file has not yet been modified for privacy-preservation, for
    # ease-of-use in the tidyverse, and for mitigation of the Excel-editing
    # hazard to the precision of its datestamps and geolocation.
    tbl <- read_csv(
      here::here("data-raw/eNV200noac50kWh", filnm_list[i]),
      name_repair = "unique_quiet",
      col_types =
        cols(`Date/Time` =
               col_datetime("%d/%m/%Y %H:%M:%S"))
    )
    tbl <- tbl |>
      select(!starts_with("Debug")) |>
      #n.b. there may be multiple Debug cols in LeafSpy logs
      select(!VIN) |>
      #n.b. publishing a vin is hazardous, because it's sometimes used as a
      #self-authenticating ("security by obscurity") identifier.
      janitor::clean_names()

    #write to /data-raw
    write_csv(tbl, here::here("data-raw/eNV200noac50kWh", filnm_list[i]))

  } else {
    # this csv file has already been modified for use in this package
    tbl <- read_csv(here::here("data-raw/eNV200noac50kWh", filnm_list[i]),
                    show_col_types = FALSE)
  }

  # perform transforms which aren't dependent on our modelling parameters,
  # as an optimisation for repeated evaluations in nlm()
  tbl <- tbl |>
    mutate(delta_t = date_time - lag(date_time))

  sampling_interval <- as.double(median(tbl$delta_t, na.rm = TRUE))
  # multiple missing samples will terminate a predictive segment
  # isolated missing samples do not hugely affect our model's predictions
  # time-stamps in the logs have a precision of 1 second
  max_delta_t <- 2 * sampling_interval + 2
  tbl <- tbl |>
    mutate(delta_t = ifelse(delta_t > max_delta_t, NA, delta_t))

  # charging power (in kW)
  tbl <- tbl |>
    mutate(charging_kW = obc_out_pwr / 1000.0, .before = cp1)

  # annoyingly, pack_t3_c is uniformly NA in all my logfiles
  tbl <- tbl |>
    mutate(pack_avg_temp = rowMeans(across(c(
      pack_t1_c, pack_t2_c, pack_t4_c
    ))), .before = cp1)

  # rate of heat gain (in K/s), a very noisy but unbiased estimate.
  # n.b. there's no advantage in using a higher-order estimate, because
  # our model is an exponential smoothing and then a numeric integration
  # (cumsum) of these noisy deltas.
  tbl <- tbl |>
    mutate(delta_K_delta_t =
             (pack_avg_temp - lag(pack_avg_temp)) / delta_t,
           .before = cp1)

  tbl_list[[i]] <- tbl
}

# this clumsiness avoids the quadratic-time behaviour of R's runtime when
# $n$ elements are individually appended to a list.  In a naive coding,
# the $i$-th element is copied a total of $n-i+1$ times.
eNV200noac50kWh <- dplyr::bind_rows(tbl_list)

# warning: any update to LeafSpy may cause it to produce logfiles with a
# different set of column names, so some additional data-tidying may be
# required.

usethis::use_data(eNV200noac50kWh, overwrite = TRUE)
