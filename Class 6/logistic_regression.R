#Logistic Regression in R

#Let's use the political data once again

# Set the working directory where your file is.
setwd("/users/sgriffin//Documents//GitHub/MultivariateStats/Class 6/")
# Now, read the file data into R
politics <- read.csv("political-1.csv")
#Now, the data is stored

View(politics)

#Generate a variable
politics$republican[politics$affiliation == "republican"] <- 1
politics$republican[politics$affiliation != "republican"] <- 0

#Run a logistic regression
attach(politics)
republican_prob <- glm(republican ~ abortion+immigration+lgbtrights+deathpenalty+regulation, 
            family = "binomial",
            data = politics)
summary(republican_prob)

#interpert the independent variables' coefficients
exp(coef(republican_prob))

#interpret the y's
politics$predicted <- predict(republican_prob) #These are logged values predicted from the logistic regression
politics$rep_prob <- (exp(predict(republican_prob))) / (1+(exp(predict(republican_prob))))
View(politics)


# Run a second logistic regression
republican_prob2 <- glm(republican ~ trump_rating+abortion+immigration+lgbtrights+deathpenalty+regulation, 
                       data = politics, 
                       family = "binomial")
summary(republican_prob2)

#Interpretation of the coefficients are in log-odds (e.g., log of probability)
# For example, this model suggests that for every one unit increase in trump_rating, 
#     the log-odds of begin a republican increases by 0.20 on avg, all else held constant

#Since this interpretation is fairly useless to us, we can
# change the coefficients to more easily interpretable by taking the e to the coefficient.
# remember that ln x = e^x. That is what we are doing here.
exp(coef(republican_prob2))


#Check Assumptions

#independence (that variables are independently distributed)
#however, we can deduce that each observation is independent of each other given that they are different people.

#multicollinearity
library(car)
vif(republican_prob)
vif(republican_prob2)

#Goodness of fit tests

logLik(republican_prob)
logLik(republican_prob2)
#The larger the absolute value of the log likelihood, the worse the fit. The second model looks better.


#Likelihood Ratio Test
# If you have two logistic regressions stored, you may compare the two
install.packages("lmtest")
library(lmtest)
lrtest(republican_prob,republican_prob2)
#Here, H0 is that smaller model is better.
# If low p-value, then reject that.
# Here, the larger model is better fit.

#Psuedo R-squared
#There is no r-squared, but some have come up with similar computations
install.packages("pscl")
library(pscl)
pR2(republican_prob)
pR2(republican_prob2)  # look for 'McFadden'
# Use the McFadden as the psuedo r-square calculation



