#####
# Assignment 1 
#####

# Loading packages
library(ggplot2)
library(repmis)
library(plyr)

# Set working directory
possiblewd <- c("~/Dropbox/4_Spring_2016/2_Collaborative Data Analysis/", 
                "~/Documents/Collaborative Social Science/Collaborative Analysis Assignments/PandP_Ass1")
set_valid_wd(possiblewd)

# Load dataset
occStatusDataFrame <- as.data.frame(occupationalStatus)

# View the data
View(occStatusDataFrame)

# Summarize frequencies over origin
tapply(occStatusDataFrame$Freq, occStatusDataFrame$origin, FUN=sum)

# Summarize frequencies over destination
tapply(occStatusDataFrame$Freq, occStatusDataFrame$destination, FUN=sum)

# Create vectors
origin <- tapply(occStatusDataFrame$Freq, occStatusDataFrame$origin, FUN=sum)
hist(origin)

# Source external analysis on Titanic dataset 
source("PandP_Ass1/Analysis_1.R")

