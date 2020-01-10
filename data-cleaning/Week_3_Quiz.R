
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


# >>>
# download file
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(Url, destfile="Idaho_housing.csv", method= "curl")

# read file
Housing <- read.csv("Idaho_housing.csv")

# households greater than 10 acres & sold more than $10k agricultural products
agriculturalLogical <- Housing$ACR == 3 & Housing$AGS == 6

# get top 3
head(which(agriculturalLogical),3)

# [1] 125 238 262
# Answer: 1

##################################################################################################################

# Question 2

# Using the jpeg package read in the following picture of your instructor into R
# https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg
# Use the parameter native=TRUE. What are the 30th and 80th quantiles of the resulting data? 
#(some Linux systems may produce an answer 638 different for the 30th quantile)
 
# 1. -16776430 -15390165
# 2. 10904118 -594524
# 3. -10904118 -10575416
# 4. -15259150 -10575416

# >>>
#install.packages('jpeg')
library(jpeg)

# url and download
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(Url, destfile="image.jpg", mode='wb')

# load image
thisimage <- readJPEG("image.jpg", native=TRUE)

# get 30th and 80th quantile
quantile(thisimage, probs = c(0.3, 0.8))

#      30%       80% 
# -15259150 -10575416 

# Answer: 4

##################################################################################################################




