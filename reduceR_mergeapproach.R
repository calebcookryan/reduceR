library(dplyr)
library(chron)
library(lubridate)
load("bz_readercapture_24_cleaned.RDATA")

####FOR TIME FILTER
#Create a function to merge overlapping rows so that if the buffer contains another read the buffer grows to encapsulate it 

reduceR_overlapping_rows <- function(df) {
  # Remove rows with NA values in any column
  df <- na.omit(df)
  # Sort the dataframe by reader_site2 and start columns
  df <- df[order(df$reader_site2, df$start), ]
  
  merged_rows <- list()
  current_row <- NULL
  
  # Iterate through the rows and merge overlapping rows
  for (i in 1:nrow(df)) {
    if (is.null(current_row)) {
      current_row <- df[i, ]
    } else if (current_row$reader_site2 == df[i, ]$reader_site2 && 
               current_row$end >= df[i, ]$start &&
               current_row$reader_pit == df[i, ]$reader_pit) {
      # Merge overlapping rows
      current_row$end <- max(current_row$end, df[i, ]$end)
    } else {
      # Add the merged row to the result and update current_row
      merged_rows <- append(merged_rows, list(current_row))
      current_row <- df[i, ]
    }
  }
  
  # Add the last merged row
  merged_rows <- append(merged_rows, list(current_row))
  
  # Combine the merged rows into a dataframe
  merged_df <- do.call(rbind, merged_rows)
  
  return(merged_df)
}

# Define the reference midnight datetime (January 1, 2012, at 00:00:00)
midnight <- as.POSIXct("2012-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS")

# Calculate the difference in seconds between each read and midnight (January 1, 2012)
time_diff <- as.numeric(difftime(bz_readercapture_24_cleaned$reader_dt, midnight, units = "secs"))

# Add the calculated time difference as a new column
bz_readercapture_24_cleaned$time_diff <- time_diff

#set time_window for buffer zone around each read
time_window = 0.3

#select the data to be included in the network ##need to select as many cols as possible that arent duplicates (only one time col) 
data<- bz_readercapture_24_cleaned %>%
  select(reader_pit, genus_species, sex, reader_site2 , time_diff) %>%
  mutate(start = time_diff - time_window) %>%
  mutate(end = time_diff + time_window) %>%
  select(reader_pit, genus_species, sex, reader_site2 , start, end) 

# Merge overlapping rows in the dataframe ##change this so that it is a single mergedread, maybe dont edit or create a new col

reduced_df <- reduceR_overlapping_rows(data)
#create a col with reduce_time for the average between the start and end of the new readframe

reduced_df$reduced_time <- (reduced_df$start+reduced_df$end)/2

#size of new dataframe
nrow(reduced_df)
#proportion of reads removed followign reduceR
(nrow(data) - nrow(reduced_df))/nrow(data)

#save
save(reduced_df, file = "reduced_df.RData")
