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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Question 5.
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Subset SCC dataset by selecting column containing 'vehicle' (in SCC.Level.Two) and attach it to NEI
NEI_SCC_Vehicle_bm <- SCC %>%
  select(SCC, SCC.Level.Two) %>%
  filter(str_detect(SCC.Level.Two, fixed('vehicle', ignore_case = TRUE))) %>%
  inner_join(NEI, by = c("SCC")) %>% # joining data with NEI
  filter(fips == "24510")  # Baltimore city

png(filename = "plot5.png", width = 6, height = 6, units = "in", res = 300)

# ggplot
g <- ggplot(aes(x = year, y = Emissions), data = NEI_SCC_Vehicle_bm)
g+geom_bar(stat="identity") +
  labs(x = "Year", y = expression("PM2.5 Emissions (tons)")) +
  labs(title = "PM2.5 Emissions from Motor Vehicle Sources in Baltimore City") +
  theme(plot.title = element_text(hjust = 0.5, size=11, face="bold"))

dev.off()

# Answer: The emissions from motor vehicle sources have decreased from 1999 to 2008 in Baltimore City.
