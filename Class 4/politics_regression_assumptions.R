#Linear Regression in R

#Let's use the politics data again
# Set the working directory where your file is.
setwd("/Users/sgriffin/Documents//GitHub/MultivariateStats/Class 4/")
# Now, read the file data into R
politics <- read.csv("political.csv")
#Now, the data is stored


View(politics)

#We can run a linear regression using lm()
bush <- lm(bush_rating~abortion+immigration+lgbtrights+deathpenalty+regulation, politics)
summary(bush)
#13 percent of the variation in the bush rating can be explained by the movements of variables
#f-test is testing for significan linear relationship
#p-value means significant linear relatinship
#higher f statistic, lower p value will be 
#the more extreme the t values, the lower the p value
obama <- lm(obama_rating~abortion+immigration+lgbtrights+deathpenalty+regulation, data=politics)
summary(obama)
trump <- lm(trump_rating~abortion+immigration+lgbtrights+deathpenalty+regulation, politics)
summary(trump)


#Check for normality 
#Note that normality of variables is NOT formally an assumption
# However, it makes sense to understand whether variables are normal
# for interpretation purposes


# and possibly looking for outliers
install.packages("car")
library(car)

#It may be more useful to just Run an outlier test
outlierTest(trump) #give you observations that are outliers, low p value is an outlier

#Another useful outlier test is using a Q-Q plot of the residuals
qqPlot(trump, main="QQ Plot")
#Does this show the same outlier?


# Now, check normality of the residuals
#Second, let's plot the residuals on a histogram
resid_trump <- resid(trump) 
hist(resid_trump, 
     main="Distribution of Residuals")


# Test for Equal Variance
# want line to be straight
plot(trump) 
ncvTest(trump)
# from seonc plot (residuals vs. fitted), equality of variance a little unequal, but ncvtest (Non-Constant Variance TEst) says it is okay


# Evaluate linearity
# component + residual plot 
# purple point is middle points of errors, if purple line strays from 0 
crPlots(trump)

#independence 
# Test for Autocorrelated Errors

durbinWatsonTest(trump)
#Notice that there IS an issue with autocorrelation here, (low p value)
#however, it is not totally applicable here since it is not time series data
#Not applicable for time data
#D-W test statistics run between 0 and 4. 
#If D-W is 2.0, no autocorrelation. (this is the most desired value; if p value is under 0.05 there is an issue)
#If D-W below 2.0, positive autocorrelation.
#If D-W above 2.0, negative autocorrelation.

# Evaluate Collinearity
vif(trump) # variance inflation factors 
