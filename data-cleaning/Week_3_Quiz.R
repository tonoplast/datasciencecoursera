
# Question 1

# The American Community Survey distributes downloadable data about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
# and load the data into R. The code book, describing the variable names is here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
# Create a logical vector that identifies the households on greater than 10 acres who sold more than $10,000 worth of agriculture products. Assign that logical vector to the variable agricultureLogical. 
# Apply the which() function like this to identify the rows of the data frame where the logical vector is TRUE.

# which(agricultureLogical)
# What are the first 3 values that result?

# 1. 125, 238,262
# 2. 236, 238, 262
# 3. 59, 460, 474
# 4. 153 ,236, 388


library(dplyr)

Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(Url, destfile="Idaho_housing.csv", method= "curl")

Housing <- read.csv("Idaho_housing.csv")
agriculturalLogical <- Housing$ACR == 3 & Housing$AGS == 6
head(which(agriculturalLogical),3)

# [1] 125 238 262
# Answer: 1

