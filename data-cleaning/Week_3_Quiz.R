
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
* # Answer: 1 *


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


#############################################################################################

# Question 3

# Load the Gross Domestic Product data for the 190 ranked countries in this data set:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
 
# Load the educational data from this data set:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv

# Match the data based on the country shortcode. How many of the IDs match? Sort the data frame 
# in descending order by GDP rank (so United States is last). What is the 13th country in the resulting data frame?

# Original data sources:
# http://data.worldbank.org/data-catalog/GDP-ranking-table
# http://data.worldbank.org/data-catalog/ed-stats

# 1. 189 matches, 13th country is Spain
# 2. 190 matches, 13th country is St. Kitts and Nevis
# 3. 234 matches, 13th country is Spain
# 4. 190 matches, 13th country is Spain
# 5. 234 matches, 13th country is St. Kitts and Nevis
# 6. 189 matches, 13th country is St. Kitts and Nevis


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

# number of IDs matched
dim(Merged)[1]

# [1] 189

# subsetting 13th data, printing country name
Merged %>% arrange(desc(Ranking)) %>%
  select(Country_Name) %>%
  slice(13)

# Country_Name
# 1 St. Kitts and Nevis

# Answer: 6


#############################################################################################

# Question 4. 

# What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?
 
# 1. 32.96667, 91.91304
# 2. 23, 30
# 3. 23, 45
# 4. 133.72973, 32.96667
# 5. 30, 37
# 6. 23.966667, 30.91304

# for more decimal places
options(pillar.sigfig = 7)

# group data, summarise (getting average), subset those that has "High" in income category, and re-order output
Merged %>% 
  group_by(Income.Group) %>% 
  summarise(Avg_Ranking =  mean(Ranking)) %>% 
  filter(grepl('High', Income.Group)) %>% 
  arrange(desc(Income.Group))

# 1 High income: OECD       32.96667
# 2 High income: nonOECD    91.91304

# Answer: 1

#############################################################################################

# Question 5
# Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. How many countries
# are Lower middle income but among the 38 nations with highest GDP?
  
# 1. 18
# 2. 0
# 3. 3
# 4. 5

library(Hmisc)
library(tidyr)

Merged %>% 
  mutate(Quant_group = cut2(Ranking, g = 5)) %>% 
  group_by(Income.Group, Quant_group) %>% 
  tally() %>% 
  spread(Quant_group, n)

# # A tibble: 5 x 6
# # Groups:   Income.Group [5]
# Income.Group         `[  1, 39)` `[ 39, 77)` `[ 77,115)` `[115,154)` `[154,190]`
# <chr>                      <int>       <int>       <int>       <int>       <int>
# 1 High income: nonOECD           4           5           8           5           1
# 2 High income: OECD             18          10           1           1          NA
# 3 Low income                    NA           1           9          16          11
# 4 Lower middle income            5          13          12           8          16
# 5 Upper middle income           11           9           8           8           9

# or

# Merged %>% 
#   mutate(Quant_group = cut2(Ranking, g = 5)) %>% 
#   group_by(Income.Group, Quant_group) %>% 
#   tally() %>%
#   filter(Income.Group == "Lower middle income" & Quant_group == '[  1, 39)')
  
# Income.Group        Quant_group     n
# <chr>               <fct>       <int>
# 1 Lower middle income [  1, 39)       5

# Answer: 4



