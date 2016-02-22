#Analysis 1



# load
data(Titanic)
View(Titanic)
class(Titanic)


# Convert to data frame 
as.data.frame(Titanic)

plot(Titanic$Survived)

mean(Titanic$Freq)
