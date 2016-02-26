#Analysis 1
library(gmodels)


# load
data(Titanic)
View(Titanic)
class(Titanic)


# Convert to data frame 
titanicDataFr <- as.data.frame(Titanic)

margin.table(Titanic, 1) # Class
margin.table(Titanic, 2) # Sex
margin.table(Titanic, 3) # Age
margin.table(Titanic, 4) # Survived

prop.table(Titanic, 2) # Prop for each case

ftable(Titanic) # more attractive printing of the table (hierarchical)

# 3-Way Frequency Table
titanic_table <- xtabs(~Class+Sex+Age+Survived+Freq, data=Titanic) 
      # I have to look into formula to assess whether it can be useful
ftable(titanic_table) # print table 
summary(titanic_table) # chi-square test of indepedence



# 2-Way Cross Tabulation
CrossTable(titanicDataFr$Survived,titanicDataFr$Class)  # only works with data frames but 
                                                        # so far doesn't work...
