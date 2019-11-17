#Part 1
# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) 
# across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 
# 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
# particulate matter data from the directory specified in the 'directory' argument and returns the mean 
# of the pollutant across all of the monitors, ignoring any missing values coded as NA.


pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  # loading libraries
  library(stringr)
  library(data.table)
  
  # setting paths
  workdir <- "C:/Users/schung/Documents/Coursera/rprog_data_specdata"
  wd <- paste(workdir,directory,sep="/")
  setwd(wd)
  
  # zeropadding file name
  id_zeropad_csv = paste(str_pad(id,3,pad="0"),".csv",sep="")
  
  # merging data into one
  MergedData <- rbindlist(lapply(id_zeropad_csv,data.table::fread))
  
  # output mean
  MergedData[, lapply(.SD, mean, na.rm = TRUE), .SDcols = pollutant][[1]]
  
}


# tests
pollutantmean("specdata","sulfate", 1:10)
pollutantmean("specdata","nitrate", 70:72)
pollutantmean("specdata","nitrate", 23)
