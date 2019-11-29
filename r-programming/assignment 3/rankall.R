
# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). 
# The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is named hospital, which contains
# the hospital name, and the second column is named state, which contains the 2-character abbreviation for
# the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
# hospitals when deciding the rankings.

# Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
# that the rankhospital function handles ties.

# NOTE: For the purpose of this part of the assignment (and for efficiency), your function should NOT call
# the rankhospital function from the previous section.
# The function should check the validity of its arguments. If an invalid outcome value is passed to rankall,
# the function should throw an error via the stop function with the exact message “invalid outcome”. The num
# variable can take values “best”, “worst”, or an integer indicating the ranking (smaller numbers are better).
# If the number given by num is larger than the number of hospitals in that state, then the function should
# return NA.




# housekeeping
rm(list=ls())
options(warn=-1)
setwd("C:/Users/schung/Documents/Coursera/rprog_data_ProgAssignment3-data")

# need this library
library(dplyr)

# State = 'TX'
# outcome = "pneumonia"
# num = 10

# function starts here
rankall <- function(outcome, num = "best"){
  
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
  
  
  ## Check outcome is valid (there is no state in the input)

    # outcome
 if (!length(Picked_Var) == 1) {
    stop('invalid outcome')
    
    ##  Return hospital name in that state with lowest 30-day death rate
  } else {
    # data to be processed
    Chosen_data <- cbind(select(thisdata,Hospital.Name, State), Picked_data)
    
    # renaming variable
    names(Chosen_data)[names(Chosen_data)=="Picked_data"] <- "Rate"
    
    # Get desired output
    NA_removed_data <- Chosen_data[complete.cases(Chosen_data),] # remove NA
    sorted_data <- NA_removed_data[order(NA_removed_data$State,NA_removed_data$Rate,NA_removed_data$Hospital.Name), ] # sorting
    
    
    ## For each state, find the hospital of the given rank
    # Ranking by Rate per State
    sorted_data$Rank <- ave(sorted_data$Rate, sorted_data$State, FUN = seq_along)
    
    
    # Selecting data based on rank & "best" / "worst" logic
    chosen_num <- NULL
    ranked_data <- NULL
    
    if (num == "best") {
      # best then pick first
      chosen_num <- head(sorted_data$Rank,1)
      ranked_data <- subset(sorted_data, Rank == chosen_num)

    } else if (num == "worst") {
      
      # worst then pick last
      ranked_data <-  sorted_data %>% 
        group_by(State) %>% 
        arrange(Rank) %>%  
        slice(n())
      
      # if number, then just use the number   
    } else {
      chosen_num <- num
      ranked_data <- subset(sorted_data, Rank == chosen_num)
    }
    
   
      # removing unncessary variables for output    
    ranked_data <- subset(ranked_data, select= -c(Rate,Rank))

    
      # renaming variables to match sample output
    names(ranked_data)[names(ranked_data)=="Hospital.Name"] <- "hospital"

    
    # getting Unique States
    State <- unique(thisdata$State)
    States_table <- as.data.frame(State) # turn into a table format

    
    # merging data to get all states
    final_data <- merge(x=States_table, y=ranked_data, by = "State", all.x = TRUE)
    
    row.names(final_data) <- final_data$State # changing rowname with State
    
    final_data <- final_data[, c(2,1)]
    
    # renaming variables to match sample output
    names(final_data)[names(final_data)=="State"] <- "state"
    
    output <- final_data
    
  }
  
  return(output)
  
}

