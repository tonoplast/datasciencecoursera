# Finding the best hospital in a state
# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
# and “f” are tied for best, then hospital “b” should be returned).

# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message “invalid state”. If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message “invalid outcome”.


# housekeeping
rm(list=ls())
options(warn=-1)
setwd("C:/Users/schung/Documents/Coursera/rprog_data_ProgAssignment3-data")

# need this library
library(dplyr)


# function starts here

best <- function(state, outcome){

  ## Read outcome data (preparation)
  
  thisdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # combining outcome strings with other necessary strings to find variables
  outcomeStrings <- strsplit(outcome, "[[:space:]]")[[1]]
  otherStrings <- c("hospital","30","day","death","mortality","rates","from")
  findStrings <- paste0(append(otherStrings,outcomeStrings), collapse = "|")
  
  # creating list of variable names to match with above strings
  New_names <- tolower(gsub("\\s+", "|", gsub("[.]"," ", names(thisdata))))
  
  # this is the variable that we will use
  Picked_Var = which(New_names %in% findStrings)
  
  # turning it into numeric
  Picked_data = as.numeric(thisdata[, Picked_Var])
  
  
  ## Check that state and outcome are valid
    # state
  if (!state %in% unique(thisdata$State)) {
    stop('invalid state')
    
    # outcome
  } else if (!length(Picked_Var) == 1) {
    stop('invalid outcome')
    
    ##  Return hospital name in that state with lowest 30-day death rate
  } else {
    # data to be processed
    Chosen_data <- cbind(select(thisdata,Hospital.Name, State), Picked_data)
    
    # renaming variable
    names(Chosen_data)[names(Chosen_data)=="Picked_data"] <- "Measure"
    
    # Get desired output
    Processing_data <- Chosen_data[which(Chosen_data$State == state), ]
    agg_data <- aggregate(Hospital.Name ~ Measure, Processing_data, min)
    
    merged_data <- merge(agg_data, Processing_data)
    sorted_data <- merged_data[order(merged_data$Measure,merged_data$Hospital.Name), ] # sorting
    output <- sorted_data[1,2] # getting first value
  }

  return(output)

}
