###############
# Assignment 1 
###############

# Loading packages
library(repmis)
library(doBy)

# Set working directory
possiblewd <- c("~/Dropbox/4_Spring_2016/2_Collaborative Data Analysis/", 
                "~/Documents/Collaborative Social Science/Collaborative Analysis Assignments/PandP_Ass1")
set_valid_wd(possiblewd)

# Load the dataset 'occupationalStatus' as a dataframe
df_occStat <- as.data.frame(occupationalStatus)

# Distribution of occupational status for fathers and sons respectively
dist_fathers <- apply(occupationalStatus, 1, sum) # Origin
dist_sons <- apply(occupationalStatus, 2, sum) # Destination

# Convert distribution to relative frequencies
freq_fathers <- prop.table(dist_fathers)
freq_sons <- prop.table(dist_sons)

# Combine frequencies into a dataframe (for table)
combined_test <- data.frame(
  status = c(1, 2, 3, 4, 5, 6, 7, 8),
  f.fathers = freq_fathers,
  f.sons = freq_sons
)

### Graphs

# Create numeric vectors for occupational status for fathers and sons respectively
barplot(dist_fathers, main="Occupational status distribution for fathers", 
        xlab="Occupational status")

barplot(dist_sons, main="Occupational status distribution for sons", 
        xlab="Occupational status")

# Create table
knitr::kable(combined_test, digits = 2)

# Combined barplot
x <- rbind(dist_fathers, dist_sons)
barplot(x, 
        col=c("Darkblue", "Red"),
        main="Distribution of occupational status",
        xlab = "Occupational status categories",
        legend = c("Fathers", "Sonds"),
        ylim = c(0, 1600),
        beside = TRUE
        )

# Convert origin and destination to numeric variables
df_occStat$origin <- as.numeric(df_occStat$origin)
df_occStat$destination <- as.numeric(df_occStat$destination)

# Create value capturing occupational status differences between fathers and sons
df_occStat$difference <- df_occStat$destination - df_occStat$origin

# Summarize observations over changes in status by collapsing on differences 

collapsed <- summaryBy(Freq ~ difference, FUN = sum, data = df_occStat) 

barplot(collapsed$Freq.sum, 
        names = collapsed$difference,
        col = "Darkblue",
        ylim = c(0, 1400)
        )
