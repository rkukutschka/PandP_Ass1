###########
# Analysis on Life Cycle Savings
###########

# Loading packages
library(ggplot2)
library(dplyr)


# Set working directory
possiblewd <- c("~/Dropbox/4_Spring_2016/2_Collaborative Data Analysis/", 
                "~/Documents/Collaborative Social Science/Collaborative Analysis Assignments/PandP_Ass1")
set_valid_wd(possiblewd)

# Load Data
data("LifeCycleSavings")

#?LifeCycleSavings

#######
#I. Inspecting the data
#######

#Summary
summary(LifeCycleSavings)


# Histograms
hist(LifeCycleSavings$sr,
     ylim = c(0,30),
     main = '% savings ratio per country (averaged between 1970 and 1980)', 
     xlab = '% savings ratio', 
     ylab = 'Frequency')


hist(LifeCycleSavings$dpi,
     ylim=c(0,20),
     main = 'Disposable income (averaged between 1970 and 1980)', 
     xlab = 'Disposable income (in USD)') 
  # Right-Skewed Data


hist(LifeCycleSavings$ddpi, main = 'Growth rate of disposable income',
     xlab = 'growth rate of disposable income (in percent)')
  # Right-Skewed Data

###########
#II. Creating additional variables
###########

# Log Data
ln.dpi <- log(LifeCycleSavings$dpi)
hist(lg_dpi)

ln.ddpi <- log(LifeCycleSavings$ddpi)
hist(lg_ddpi)

# Creating Dummies for below or above the mean values

dum.old <- as.numeric(LifeCycleSavings$pop75>median(LifeCycleSavings$pop75))
dum.young <- as.numeric(LifeCycleSavings$pop15>median(LifeCycleSavings$pop15))
dum.rich <- as.numeric(LifeCycleSavings$dpi>median(LifeCycleSavings$dpi))
dum.fast <- as.numeric(LifeCycleSavings$ddpi>median(LifeCycleSavings$ddpi))

dum.fast.foo <- as.character(combined_df$dum.fast)
dum.young.foo <- as.character(combined_df$dum.young)

# Combine variables to new data frame
combined_df <- data.frame(LifeCycleSavings, ln.ddpi, ln.dpi, dum.old,
                          dum.young, dum.rich, dum.fast, dum.fast.foo, 
                          dum.young.foo)

#############
#III. Analysis
#############

### Main variable of interest: Savings Ratio (sr)

boxplot(combined_df$sr, main = '% savings ratio per country (averaged btw 1970 and 1980)')
summary(combined_df$sr)
sd(combined_df$sr)

  # 75% of the country observations have an average savings rate between 6.97 and 12.62% 
  # The distribution is slightly left-skewed (higher median than mean).
  # Thus, a log-transformation would not help here.

### Main explanatory variable: disposable income (dpi)

boxplot(combined_df$dpi, main = 'Distribution of disposable income (in USD)')
summary(combined_df$dpi)
sd(combined_df$dpi)

  # 75% of the country observations have an average disposable income between 288,2 and 
  # 1796 USD a month
  # The distribution is strongly right-skewed (higher mean than median).
  # Therefore, we will use the log-transformation.

### Correlation of sr and dpi

# Plot out the variables
ggplot2::ggplot(combined_df, aes(ln.dpi, sr)) +
  geom_point(shape=1) + geom_smooth(method=lm) + theme_bw()

# Using loess instead of linear regression line
ggplot2::ggplot(combined_df, aes(ln.dpi, sr)) +
  geom_point(shape=1) + geom_smooth() + theme_bw()

# Testing for correlation
cor.test(combined_df$ln.dpi, combined_df$sr)
  # The variables are positively correlated (0.28). 
  # The correlation is not significant at the 95% level. 


### Testing the correlation for conditions

# Condition: Faster growing disposable income
ggplot2::ggplot(combined_df, aes(ln.dpi, sr, color=dum.fast.foo)) +
  geom_point(shape=1) + scale_colour_hue(l=50) + 
  geom_smooth(method=lm) 

# Condition: Younger population
ggplot2::ggplot(combined_df, aes(ln.dpi, sr, color=dum.young.foo)) +
  geom_point(shape=1) + scale_colour_hue(l=50) + 
  geom_smooth(method=lm)



### Boxplot
boxplot(combined_df$sr~combined_df$dum.rich)
  # Richer countries (dpi higher than median) have a much more compressed 
  # distribution of savings rates and on average a higher savings rate.

boxplot(combined_df$sr~combined_df$dum.old)
  # Countries with a larger share of old people (pop75 higher than median)  have 
  # a much more compressed distribution of savings rates and on average a higher savings rate.
boxplot(combined_df$sr~combined_df$dum.young)



