
# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. For example, the call
# rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. Hospitals that do not have data on a particular outcome should
# be excluded from the set of hospitals when deciding the rankings.
# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken by using the hospital name. For example, in Texas (“TX”),
# the hospitals with lowest 30-day mortality rate for heart failure are shown here.
# 
# > head(texas)
# Hospital.Name Rate Rank
# 3935 FORT DUNCAN MEDICAL CENTER 8.1 1
# 4085 TOMBALL REGIONAL MEDICAL CENTER 8.5 2
# 4103 CYPRESS FAIRBANKS MEDICAL CENTER 8.7 3
# 3954 DETAR HOSPITAL NAVARRO 8.7 4
# 4010 METHODIST HOSPITAL,THE 8.8 5
# 3962 MISSION REGIONAL MEDICAL CENTER 8.8 6
# Note that Cypress Fairbanks Medical Center and Detar Hospital Navarro both have the same 30-day rate
# (8.7). However, because Cypress comes before Detar alphabetically, Cypress is ranked number 3 in this
# scheme and Detar is ranked number 4. One can use the order function to sort multiple vectors in this
# manner (i.e. where one vector is used to break ties in another vector).
# 
# The function should check the validity of its arguments. If an invalid state value is passed to rankhospital,
# the function should throw an error via the stop function with the exact message “invalid state”. If an invalid
# outcome value is passed to rankhospital, the function should throw an error via the stop function with
# the exact message “invalid outcome”.



# housekeeping
rm(list=ls())
options(warn=-1)
setwd("C:/Users/schung/Documents/Coursera/rprog_data_ProgAssignment3-data")

# need this library
library(dplyr)

# function starts here
rankhospital <- function(state, outcome, num = "best"){
  
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
    names(Chosen_data)[names(Chosen_data)=="Picked_data"] <- "Rate"
    
    # Get desired output
    Processing_data <- Chosen_data[which(Chosen_data$State == state), ] # pick State
    NA_removed_data <- Processing_data[complete.cases(Processing_data),] # remove NA
    sorted_data <- NA_removed_data[order(NA_removed_data$Rate,NA_removed_data$Hospital.Name), ] # sorting
    
    # Adding rank by sorted rate
    sorted_data$Rank <- 1:nrow(sorted_data)
    
    # Selecting data based on rank & "best" / "worst" logic
    chosen_num <- NULL
    if (num == "best") {
      chosen_num <- head(sorted_data$Rank,1) # best then pick first
    } else if (num == "worst") {
      chosen_num <- tail(sorted_data$Rank,1) # worst then pick last
    } else {
      chosen_num <- num
    }
    
    output <- sorted_data[chosen_num,1] # getting chosen rank value
  }
  
  return(output)
  
}


# examples
rankhospital("TX", "heart failure", 4)
[1] "DETAR HOSPITAL NAVARRO"

rankhospital("MD", "heart attack", "worst")
[1] "HARFORD MEMORIAL HOSPITAL"

