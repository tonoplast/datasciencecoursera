
# directory <- "specdata"


corr <- function(directory, threshold=0) {
  # loading libraries
  library(stringr)
  library(data.table)
  library(dplyr)
  
  # setting paths
  workdir <- "C:/Users/schung/Documents/Coursera/rprog_data_specdata"
  wd <- paste(workdir,directory,sep="/")
  setwd(wd)
  
  id=1:332
  
  # zeropadding file name
  id_zeropad_csv = paste(str_pad(id,3,pad="0"),".csv",sep="")
  
  # merging data into one
  MergedData <- rbindlist(lapply(id_zeropad_csv,data.table::fread))

  # getting complete data
  CompleteLogic <- complete.cases(MergedData)
  CompleteData <- MergedData[CompleteLogic]
  
  
  # output correlations with threshold
  FinalData <- CompleteData[, .(nobs = .N, CorrData = cor(x = sulfate, y = nitrate)), by = ID][nobs > threshold]
  return(FinalData[, CorrData])
  
}
