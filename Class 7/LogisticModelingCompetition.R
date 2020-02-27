#Logistic Regression in R

#Let's use the political data once again

# Set the working directory where your file is.
setwd("/users/sgriffin//Documents//GitHub/MultivariateStats/Class 7/")
# Now, read the file data into R
df <- read.csv("LogisticModeling.csv")
#Now, the data is stored

View(df)
summary(df$RACE.ETH)
summary(df$HHINCOME)

#Generate a variable
df$Asian[df$RACE.ETH == "A"] <- 1
df$Asian[df$RACE.ETH != "A"] <- 0
df$Black[df$RACE.ETH == "B"] <- 1
df$Black[df$RACE.ETH != "B"] <- 0
df$Hispanic[df$RACE.ETH == "H"] <- 1
df$Hispanic[df$RACE.ETH != "H"] <- 0
df$White[df$RACE.ETH == "W"] <- 1
df$White[df$RACE.ETH != "W"] <- 0

df$Income[df$HHINCOME >= "45700"] = 1
df$Income[df$HHINCOME < "45700"] = 0
# df$old[df$AGE >= "80"] = 1
# df$old[df$AGE < "80"] = 0




#Run a logistic regression
attach(df)
# medicare_prob <- glm(MEDICARE ~ AGE+HHINCOME+GENDER+Asian+Black+Hispanic+White, 
#                        family = "binomial",
#                        data = df) 
# summary(medicare_prob)
# 
# #interpert the independent variables' coefficients
# exp(coef(medicare_prob))
# 
# #interpret the y's
# df$predicted <- predict(medicare_prob) #These are logged values predicted from the logistic regression
# df$medicare_prob <- (exp(predict(medicare_prob))) / (1+(exp(predict(medicare_prob))))
# View(df)

# Run a second logistic regression
medicare_prob2 <- glm(MEDICARE ~ AGE+CAN+SYS+Income, 
                     family = "binomial",
                     data = df) 
summary(medicare_prob2)
exp(coef(medicare_prob2))
df$predicted2 <- predict(medicare_prob2) #These are logged values predicted from the logistic regression
df$medicare_prob2 <- (exp(predict(medicare_prob2))) / (1+(exp(predict(medicare_prob2))))
View(df)

df$score[df$predicted2 >= 0.5] = 1
df$score[df$predicted2 < 0.5] = 0

sum(df$score-df$MEDICARE == 0)

#multicollinearity
library(car)
vif(medicare_prob)
vif(medicare_prob2)

#Goodness of fit tests

logLik(medicare_prob)
logLik(medicare_prob2)
#The larger the absolute value of the log likelihood, the worse the fit. The second model looks better.


#Likelihood Ratio Test
# If you have two logistic regressions stored, you may compare the two
install.packages("lmtest")
library(lmtest)
lrtest(medicare_prob,medicare_prob2)
#Here, H0 is that smaller model is better.
# If low p-value, then reject that.
# Here, the larger model is better fit.

#Psuedo R-squared
#There is no r-squared, but some have come up with similar computations
install.packages("pscl")
library(pscl)
pR2(medicare_prob)
pR2(medicare_prob2)  # look for 'McFadden'
# Use the McFadden as the psuedo r-square calculation