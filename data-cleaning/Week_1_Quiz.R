# Question 1
# The American Community Survey distributes downloadable data about United States communities. Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
# and load the data into R. The code book, describing the variable names is here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
# How many properties are worth $1,000,000 or more?


library(drlyr)

# Loading file
mydata <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")

# converting into table format
mytable <- tbl_df(mydata)

# counting -- Filtering NA and VAL (property value) according to question
mytable %>%
  filter(!is.na(VAL), VAL >= 24) %>%
  summarize(count= n())
  
# Asnwer: 53


#########################################################################################################

# Question 2
# Use the data you loaded from Question 1. Consider the variable FES in the code book. Which of the "tidy data" principles does this variable violate?

# 1. Each variable in a tidy data set has been transformed to be interpretable.
# 2. Tidy data has no missing values.
# 3. Numeric values in tidy data can not represent categories.
# 4. Tidy data has one variable per column.

# Answer: 4

#########################################################################################################

# Question 3
# Download the Excel spreadsheet on Natural Gas Aquisition Program here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx
# Read rows 18-23 and columns 7-15 into R and assign the result to a variable called:
# dat

# What is the value of:
# sum(dat$Zip*dat$Ext,na.rm=T)
# (original data source: http://catalog.data.gov/dataset/natural-gas-acquisition-program)

# 1. 33544718
# 2. NA
# 3. 154339
# 4. 36534720

library(xlsx)

# donwloading file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl, paste0(getwd(),"/NGAP.xlsx"), method = "curl")

# assigning rows, cols, and loading the section of interest
Rows <- 18:23
Cols <- 7:15
dat <- read.xlsx(file="NGAP.xlsx",sheetIndex=1, colIndex = Cols, startRow = first(Rows), endRow = last(Rows),  header = TRUE)

# doing the sum
sum(dat$Zip*dat$Ext,na.rm=T)

# Answer: 4

#########################################################################################################

# Question 4
# Read the XML data on Baltimore restaurants from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml
# How many restaurants have zipcode 21231?

# 1. 127
# 2. 17
# 3. 100
# 4. 181


library(XML)

# donwloading file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(fileUrl, paste0(getwd(),"/restaurants.xml"), method = "curl")

# xml functions
mydata <- xmlTreeParse(file="restaurants.xml", useInternal=TRUE)
rootNode <- xmlRoot(mydata)

# find zipcodes
zipcode <- xpathSApply(rootNode, "//zipcode", xmlValue)

# get answer
length(zipcode[zipcode == 21231])

# Answer: 1

#########################################################################################################



