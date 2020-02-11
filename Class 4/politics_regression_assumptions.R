#Linear Regression in R

#Let's use the politics data again
# Set the working directory where your file is.
setwd("C:/Users/echan1/Dropbox/QTM 3610 Sp 2019/")
# Now, read the file data into R
politics <- read.csv("political.csv")
#Now, the data is stored


View(politics)

#We can run a linear regression using lm()
bush <- lm(bush_rating~abortion+immigration+lgbtrights+deathpenalty+regulation, politics)
summary(bush)
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
outlierTest(trump)

#Another useful outlier test is using a Q-Q plot of the residuals
qqPlot(trump, main="QQ Plot")
#Does this show the same outlier?


# Now, check normality of the residuals
#Second, let's plot the residuals on a histogram
resid_trump <- resid(trump) 
hist(resid_trump, 
     main="Distribution of Residuals")


# Test for Equal Variance
plot(trump)
ncvTest(trump)
# from seonc plot (residuals vs. fitted), equality of variance a little unequal, but ncvtest (Non-Constant Variance TEst) says it is okay


# Evaluate linearity
# component + residual plot 
crPlots(trump)

#independence 
# Test for Autocorrelated Errors
durbinWatsonTest(trump)
#Notice that there IS an issue with autocorrelation here
#however, it is not totally applicable here since it is not time series data
#Not applicable for time data
#D-W test statistics run between 0 and 4. 
#If D-W is 2.0, no autocorrelation.
#If D-W below 2.0, positive autocorrelation.
#If D-W above 2.0, negative autocorrelation.

# Evaluate Collinearity
vif(trump) # variance inflation factors 
