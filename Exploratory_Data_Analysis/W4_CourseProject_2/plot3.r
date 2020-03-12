# Fine particulate matter (PM2.5) is an ambient air pollutant for which there is 
# strong evidence that it is harmful to human health. In the United States, 
# the Environmental Protection Agency (EPA) is tasked with setting national ambient 
# air quality standards for fine PM and for tracking the emissions of this pollutant 
# into the atmosphere. Approximatly every 3 years, the EPA releases its database on 
# emissions of PM2.5. This database is known as the National Emissions Inventory (NEI). 
# You can read more information about the NEI at the EPA National Emissions Inventory web site.
# 
# For each year and for each type of PM source, the NEI records how many tons of PM2.5 
# were emitted from that source over the course of the entire year. The data that you will 
# use for this assignment are for 1999, 2002, 2005, and 2008.


## a placeholder for current directory (if re-running the code)

if (exists("pwd")) {
  setwd(pwd)
}

# clear environment
rm(list=ls())

# packages used
library(dplyr)
library(ggplot2)
library(stringr)

## a placeholder for current directory
pwd <- getwd()

# set working directory
setwd("C:/Users/schung/Documents/Coursera/ExploratoryDataAnalysis_W4_Course_Project_2")

# downloading zip file
this_file <- "pm25_data.zip"
this_Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(this_Url, this_file, method= 'curl')

# unzip file
unzip(this_file)

# reading data
NEI <- readRDS("summarySCC_PM25.RDS") %>%
  mutate(year = as.factor(year)) # making year factor to not mess with graph later

SCC <- readRDS("Source_Classification_Code.RDS") %>%
  mutate(SCC = as.character(SCC)) # making SCC character to avoid warning when merging later


# Question 3.
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

# subset data by Baltimore city
Baltimore_PM25 <- NEI %>%
  filter(fips == "24510") 
  

png(filename = "plot3.png", width = 8, height = 6, units = "in", res = 300)

# ggplot
g <- ggplot(aes(x = year, y = Emissions, fill = type), data = Baltimore_PM25)
g+geom_bar(stat="identity") +
  facet_grid(.~type) +
  guides(fill = FALSE) +
  labs(x = "Year", y = "PM2.5 Emissions (tons)") +
  labs(title = "PM2.5 Emissions in the Baltimore City by Source Type") +
  theme(plot.title = element_text(hjust = 0.5, size=11, face="bold"))

dev.off()

# Answer: The 'non-road', 'nonpoint' and 'on-road' sources have seen decreases in emission from 1999 to 2008 in Baltimore City.
#         The 'point' source have seen increase in emission from 1999 to 2005 in Baltimore City, but decreased by 2008.
