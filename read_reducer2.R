library("openxlsx")
library("readxl")
library("dplyr")
library("chron")


#species_name = "rotundus"
#species_data = read_excel(paste("/Users/felixlawson/Desktop/stoof/UW/Graduate work/thesis stuff/Chapter 1/data/Reader data/species data/",species_name,".xlsx", sep=""))
species_data = read_excel("/Users/felixlawson/Desktop/stoof/UW/Graduate work/thesis stuff/Chapter 1/data/Reader data/species data/reduced reads/test_3oct2024.xlsx")
total_reads <- nrow(species_data)

# The time used to determine separate reads 
tol <- second(0.3)
start_index <- 1
# change time into a format that time can be used algebraically
prev_time <- as.POSIXct(unlist(species_data[start_index,]["reader_dt"]))
reduced_reads <- list()
# The ID of the first read in the file
new_id <- unlist(species_data[start_index,]["reader_pit"])
# The location of the first read in the file
new_location <- unlist(species_data[start_index,]["reader_site2"])

for (i in 2:total_reads){
  # change time into a format that time can be used algebraically
  # get the time of the current read
  curr_time <- as.POSIXct(unlist(species_data[i,]["reader_dt"]))
  diff <- as.numeric(difftime(curr_time, prev_time, units="secs"))
  
  if (new_id == unlist(species_data[i,]["reader_pit"]) & new_location == unlist(species_data[i,]["reader_site2"])){
    if (diff > tol){
      #make summary of data here
      # I am looking for the midpoint of the group of reads that were consistently read at most the tolerance apart
      # This isn't perfect but it will significantly reduce the reads
      midpoint <- as.integer((i - start_index)/2) + start_index
      start_index <- i
      reduced_reads <- c(reduced_reads, midpoint)
    }
  }
  # if location or ID changes make a new group of reads
  else{
    new_id <- unlist(species_data[i,]["reader_pit"])
    new_location <- unlist(species_data[i,]["reader_site2"])
    midpoint <- as.integer((i - start_index)/2) + start_index
    start_index <- i
    reduced_reads <- c(reduced_reads, midpoint)
  }
  if(i == total_reads){
    reduced_reads <- c(reduced_reads, i)
  }
  # get the time of the current read as it will be the previous time on the next loop
  prev_time <- as.POSIXct(unlist(species_data[i,]["reader_dt"]))
  
}

reduced_reads <- unlist(reduced_reads)
print(paste("Total reads before reduction:", total_reads))
print(paste("Total reads after reduction:", length(reduced_reads)))

#write.xlsx(species_data[reduced_reads,], file= paste("/Users/felixlawson/Desktop/stoof/UW/Graduate work/thesis stuff/Chapter 1/data/Reader data/species data/reduced reads/",species_name,"_reduced.xlsx", sep=""))
write.xlsx(species_data[reduced_reads,], file="/Users/felixlawson/Desktop/stoof/UW/Graduate work/thesis stuff/Chapter 1/data/Reader data/species data/reduced reads/test_3oct2024_reduced.xlsx")