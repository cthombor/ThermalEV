## code to prepare `eNV200noac50kWh` dataset

# usethis::use_data_raw("eNV200noac50kWh")

eNV200noac50kWh <- 0

usethis::use_data(eNV200noac50kWh, overwrite = TRUE)

setwd("C:/Users/Luke/Documents/NBA_Leaders")

logtibble <- read_csv(
  paste0("data-raw/", logfile, ".csv"),
  name_repair = "unique_quiet",
  col_types =
    cols(`Date/Time` =
           col_datetime("%d/%m/%Y %H:%M:%S"))
)
logtibble <- logtibble |>
  select(!starts_with("Debug")) |>
  janitor::clean_names()
#n.b. there are multiple Debug cols in LeafSpy logs

# from https://rpubs.com/LMunyan/363306:

file_list <- list.files(path="C:/Users/Luke/Documents/NBA")

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

setwd("C:/Users/Luke/Documents/NBA_Leaders")

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
