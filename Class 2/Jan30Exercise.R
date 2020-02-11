setwd("/Users/sgriffin/Documents/Github/MultiStats")

huddata <- read.csv("hud_data.CSV")
View(huddata)
hud_na = subset(huddata, totsal>=0)
mean(hud_na$totsal)




#Questions1
# mean(y, na.rm = TRUE)

huddata$newTotSal = replace(huddata$totsal, which(huddata$totsal<0),NA)
View(huddata$newTotSal)
attach(huddata)
# x <- Copies
y <- totsal
y[ y < 0 ] <- NA
mean(y, na.rm = TRUE)
summary(y, na.rm=TRUE)
median(y, na.rm = TRUE)
sd(y, na.rm = TRUE)


#Question2
# library(Hmisc)
# Now, we can use the Hmisc package.
library(Hmisc, warn.conflicts = FALSE)
describe(y)
# Histogram
hist(y,main = "Histogram of household salary", xlab = "Household Salary Amt")

#Question3
# attach(huddata)
# x <- Copies
huddata$zerosal[huddata$newTotSal == 0] = 1
huddata$zerosal[huddata$newTotSal != 0 ] = 0
anyNA(huddata$zerosal)
summary(huddata$zerosal)

plot(huddata$age, huddata$newTotSal)
cor(huddata$age, huddata$newTotSal, use='complete.obs')

huddata$burden = huddata$cost08/huddata$totsal
summary(huddata$burden)

#Question 4: How many (and percentage of) households has missing salaries? What do you think is the indicator for missing salaries in this data? Why do you think there are missing salaries? Why do you think the indicator for missing salaries is what it is.
# zero_values = count(huddata$totsal, 0)
# totsal_percent <- zero_values/rowSums((totsal_percent))
  


#Question 5: Create a scatterplot showing head of household age (‘age’) on the X-Axis and salary (‘totsal’) on the Y-Axis. Add a regression line. Calculate the correlation between the two variables. Then, run a simple linear regression. What can you tell about the relationship between the two variables?
  


