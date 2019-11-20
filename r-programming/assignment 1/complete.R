# Part 2
# Write a function that reads a directory full of files and reports the number of completely 
# observed cases in each data file. The function should return a data frame where the first column is 
# the name of the file and the second column is the number of complete cases.

# directory <- "specdata"


complete <- function(directory, id = 1:332) {
  # loading libraries
  library(stringr)
  library(data.table)
  library(dplyr)

  # setting paths
  workdir <- "C:/Users/schung/Documents/Coursera/rprog_data_specdata"
  wd <- paste(workdir,directory,sep="/")
  setwd(wd)
  
  # zeropadding file name
  id_zeropad_csv = paste(str_pad(id,3,pad="0"),".csv",sep="")
  
  # merging data into one
  MergedData <- rbindlist(lapply(id_zeropad_csv,data.table::fread))
  names(MergedData)[names(MergedData) == "ID"] <- "id"
  
  # getting complete data
  CompleteData <- complete.cases(MergedData)
  FinalData <- MergedData[CompleteData]
  
  # output rowcount
  FinalData[, .(nobs = .N), by = id]
  

}
