## concatenate csv files in data-raw/eNV200noac50kWh/ to create
## data/eNV200noac50kWh.csv

# warning: this script modifies csv files, deleting any "vin" column to
# maintain the security-by-obscurity defense against malicious use of this
# identifier.

# warning: this script interprets the entries in the Date/Time column of
# a csv file as "%d/%m/%Y %H:%M:%S", rewriting these as timestamps in
# in POSIXct format ("%Y-%m-$d %H:%M:%S) in a date_time column.

# warning: this script muntifies the column-headers in csv files,
# so that they're fully acceptable as column names in the tidyverse.

# warning: this script deletes any Debug columns in the csv files.  V1.5.0
# of LeafSpy for Android has two such columns.  Duplicate column names
# are second-class entities in the tidyverse.

# n.b. it is dangerous to use Excel to edit a csv file from LeafSpy,
# because precision is lost from timestamps and lat/long information
# when they are truncated when writing a csv file from Excel -- unless you
# defang this default "feature" of Excel.  See
# leafspy.com/wp-content/uploads/2024/04/LeafSpy-Help-1.5.0.pdf

# Run once, when adding this dataset to the package:
# usethis::use_data_raw("eNV200noac50kWh")
# Warning: this command overwrites any existing eNV200noac50kWh.R.

library(tidyverse)
library(usethis)
library(xts)
library(janitor)

cdir <- getwd()
setwd(paste0(cdir, "/data-raw/eNV200noac50kWh"))

file_list = list.files()

for (i in seq(length(file_list))) {
  tbl <- read_csv(
    file_list[1],
    name_repair = "unique_quiet",
    col_types =
      cols(`Date/Time` =
             col_datetime("%d/%m/%Y %H:%M:%S"))
  )
  tbl <- tbl |>
    select(!starts_with("Debug")) |>
    select(!vin) |>
    janitor::clean_names()
  #n.b. there may be multiple Debug cols in LeafSpy logs
  #n.b. publishing a vin is hazardous, because it's sometimes used as a
  #self-authenticating ("security by obscurity") identifier.

  write.csv(tbl, file_list[1])


}



usethis::use_data(eNV200noac50kWh, overwrite = TRUE)




#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- read_excel(file_list[i], range = cell_cols("A:H")) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$Class <- sapply(strsplit(gsub(".xlsx", "", file_list[i]), "_"), function(x){x[2]}) #clean the data as needed, in this case I am creating a new column that indicates which file each row of data came from
  dataset <- rbind(dataset, temp_data) #for each iteration, bind the new data to the building dataset
}

# also from https://rpubs.com/LMunyan/363306:
require(data.table)

#create a list of the files from your target directory
file_list <- list.files(path="C:/Users/Luke/Documents/NBA_Leaders")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F) #read in files using the fread function from the data.table package
  dataset <- rbindlist(list(dataset, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

# from https://rpubs.com/jimhester/rbind

f2 <- function(days) {
  results <- vector("list", length(days))
  i <- 1
  for (d in days) {
    # Get data
    day_data <- get_data(d)

    # Process data
    # ...

    # Store data
    results[[i]] <- day_data
    i <- i + 1
  }
  as.data.frame(dplyr::bind_rows(results))
}

