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

# Combine frequencies into a table (data-frame format)
frequency_table <- data.frame(
  Category = c(1, 2, 3, 4, 5, 6, 7, 8),
  Fathers = freq_fathers,
  Sons = freq_sons
)

### Graphs

# Create table
knitr::kable(frequency_table, digits = 2)

# Combined barplot of occupational status distribution for fathers and sons
dist_combined <- rbind(dist_fathers, dist_sons)
barplot(dist_combined, 
        col=c("navyblue", "darkkhaki"),
        main="Distribution of occupational status",
        xlab = "Occupational status categories",
        legend = c("Fathers", "Sons"),
        ylim = c(0, 1600),
        beside = TRUE
)

# Convert origin and destination to numeric variables
df_occStat$origin <- as.numeric(df_occStat$origin)
df_occStat$destination <- as.numeric(df_occStat$destination)

# Create value capturing occupational status differences between fathers and sons
df_occStat$difference <- df_occStat$destination - df_occStat$origin

# Summarize observations over changes in status by collapsing on differencesand create barplot
collapsed <- summaryBy(Freq ~ difference, FUN = sum, data = df_occStat) 

barplot(collapsed$Freq.sum, 
        names = collapsed$difference,
        col = "Navyblue",
        ylim = c(0, 1400)
        )
