
# Let's import the CSV file
# Set the working directory first (different from yours)
# Set the working directory where your file is.
setwd("/Users/sgriffin/Documents/GitHub/MultivariateStats/Class 4/")
# Now, read the file data into R
politics <- read.csv("political.csv")
#Now, the data is stored


View(politics)

install.packages("mvnormtest")
library(mvnormtest)

politics_2 <- politics[c(2:9)]
politics_t <- t(politics_2)

politics_t <- t(politics[c(2:9)])

#Run the Shapiro-Wilk Test
mshapiro.test(politics_t)

install.packages("normwhn.test")
library(normwhn.test)

normality.test1(politics_2)

#OR another way
install.packages("dplyr")
install.packages("ggpubr")
library(dplyr)
library(ggpubr)

attach(politics_2)
ggdensity(politics_2$bush_rating)
ggdensity(obama_rating)
ggdensity(trump_rating)
ggdensity(politics_2$abortion)
ggdensity(politics_2$immigration)
ggdensity(politics_2$lgbtrights)
ggdensity(politics_2$deathpenalty)
ggdensity(politics_2$regulation)

library(ggpubr)
ggqqplot(politics_2$bush_rating)
ggqqplot(politics_2$obama_rating)
ggqqplot(politics_2$trump_rating)
ggqqplot(politics_2$abortion)
ggqqplot(politics_2$immigration)
ggqqplot(politics_2$lgbtrights)
ggqqplot(politics_2$deathpenalty)
ggqqplot(politics_2$regulation)

#run shapiro-wilks test for EACH variable.
shapiro.test(politics_2$bush_rating)
shapiro.test(politics_2$obama_rating)
shapiro.test(politics_2$trump_rating)
shapiro.test(politics_2$abortion)
shapiro.test(politics_2$immigration)
shapiro.test(politics_2$lgbtrights)
shapiro.test(politics_2$deathpenalty)
shapiro.test(politics_2$regulation)

#determinant of covariance and correlation matrices
pol_cov <- cov(politics_2)
pol_cor <- cor(politics_2)

det(pol_cov)
det(pol_cor)

#equality across group's covariance matrices
install.packages("biotools", dependencies = TRUE)
library(biotools)
boxM(politics[,-1],politics[,1])