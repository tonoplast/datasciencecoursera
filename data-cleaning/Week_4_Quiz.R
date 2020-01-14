# Question 1
#
# The American Community Survey distributes downloadable data about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
# and load the data into R. The code book, describing the variable names is here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
# 
# Apply strsplit() to split all the names of the data frame on the characters "wgtp". 
# What is the value of the 123 element of the resulting list?
# 
# 1. "w" "15"
# 2. "wgtp" "15"
# 3. "wgt" "15"
# 4. "" "15"

# >>>
# download file
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(Url, destfile="US_communities.csv", method= "curl")

# read csv
US_communities <- read.csv("US_communities.csv")

strsplit(names(US_communities), "wgtp")[123]

# [1] ""   "15"

# Answer: 4

#############################################################################################

# Question 2
# 
# Load the Gross Domestic Product data for the 190 ranked countries in this data set:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
# 
# Remove the commas from the GDP numbers in millions of dollars and average them. What is the average?
#   
#   Original data sources:
#   
#   http://data.worldbank.org/data-catalog/GDP-ranking-table
# 
# 1. 377652.4
# 2. 379596.5
# 3. 387854.4
# 4. 381668.9

# >>>

# clear environment
rm(list=ls())

# load packages
library(dplyr)

# download file
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(Url, destfile="dom_pro.csv", method= "curl")

# read csv
dom_pro <- read.csv("dom_pro.csv", na.strings = c("","NA"), stringsAsFactors = FALSE)

# cleaning GDP data (remove unncessary columns and rows, and renaming colnames)
dom_pro <- dom_pro %>% 
  select(c(1,2,4,5)) %>% 
  na.omit()
colnames(dom_pro) <- c("CountryCode","Ranking","Country_Name","Value")

# Convert to number (remove comma)
dom_pro$Value <- as.numeric(gsub(",","",dom_pro$Value))

# Average GDP
dom_pro %>% summarise(mean(Value))

# 1    377652.4

# Answer: 1

#############################################################################################

# Question 3
# 
# In the data set from Question 2 what is a regular expression that would allow you to 
# count the number of countries whose name begins with "United"? 
# Assume that the variable with the country names in it is named countryNames. 
# How many countries begin with United?
#   
# 1. grep("*United",countryNames), 2
# 2. grep("^United",countryNames), 3
# 3. grep("*United",countryNames), 5
# 4. grep("United$",countryNames), 3

# >>>

countries_begins_with_united <- dom_pro$Country_Name[grep("^United",dom_pro$Country_Name)]
print(countries_begins_with_united)
length(countries_begins_with_united)

# [1] "United States"        "United Kingdom"       "United Arab Emirates"
# [1] 3

# Answer: 2

#############################################################################################

# Question 4
# 
# Load the Gross Domestic Product data for the 190 ranked countries in this data set:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
# 
# Load the educational data from this data set:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv
# 
# Match the data based on the country shortcode. 
# Of the countries for which the end of the fiscal year is available, how many end in June?
# 
# Original data sources:
# http://data.worldbank.org/data-catalog/GDP-ranking-table
# http://data.worldbank.org/data-catalog/ed-stats
# 
# 1. 7
# 2. 15
# 3. 13
# 4. 16

# >>>

# clear environment
rm(list=ls())

library(dplyr)

# download files
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(Url, destfile="GDP_190.csv", method= "curl")

Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(Url, destfile="EDU.csv", method= "curl")

# read csv
GDP_190 <- read.csv("GDP_190.csv", na.strings = c("","NA"), stringsAsFactors = FALSE)
EDU <- read.csv("EDU.csv", na.strings = c("","NA"), stringsAsFactors = FALSE)

# cleaning GDP data (remove unncessary columns and rows, and renaming colnames)
GDP_190 <- GDP_190 %>% 
  select(c(1,2,4,5)) %>% 
  na.omit()
colnames(GDP_190) <- c("CountryCode","Ranking","Country_Name","Value")

# Turning character into numeric (because of commas, gsub was used to remove prior)
GDP_190$Ranking <- as.numeric(gsub(",","",GDP_190$Ranking))
GDP_190$Value <- as.numeric(gsub(",","",GDP_190$Value))

# merging two tables
Merged <- merge(EDU, GDP_190, by ="CountryCode")

# Fiscal Year in Special.Notes (count)
length(grep('fiscal year end.*june', tolower(Merged$Special.Notes)))

# [1] 13

# Answer: 3

#############################################################################################

# Question 5
# 
# You can use the quantmod (http://www.quantmod.com/) package to get historical stock prices for 
# publicly traded companies on the NASDAQ and NYSE. 
# Use the following code to download data on Amazon's stock price and get the times the data was sampled.
# 
# library(quantmod)
# amzn = getSymbols("AMZN",auto.assign=FALSE)
# sampleTimes = index(amzn)
# 
# How many values were collected in 2012? How many values were collected on Mondays in 2012?
# 
# 1. 250, 47
# 2. 252, 50
# 3. 250, 51
# 4. 251, 47

# >>>
# clear environment
rm(list=ls())

# install.packages("quantmod")
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

library(dplyr)
library(lubridate)

# Extract Date
date_collected <- index(amzn)

# Extract Year
year_collected <- year(date_collected)

# Extract Day
day_collected <- weekdays(date_collected)

# making dataframe using Year and Day
df <- as.data.frame(table(year_collected, day_collected))

# How many values in 2012
df %>% group_by(year_collected) %>%
  summarise(SUM_year = sum(Freq)) %>%
  filter(year_collected == 2012)

# 1 2012                250

# How many values on Mondays 2012
df %>% group_by(year_collected, day_collected) %>%
  summarise(SUM_day = sum(Freq)) %>%
  filter(year_collected == 2012, day_collected == "Monday")

# 1 2012           Monday             47

# Answer: 1
