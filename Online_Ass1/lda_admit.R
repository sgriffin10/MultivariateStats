
#get admissions data to MBA program from a website
url <- 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/admission.csv'
admit <- read.csv(url)

#View data
View(admit)

#Check the assumptions
#Box M Test
install.packages("biotools")
library(biotools)
boxM(admit[1:2],admit[,3])
#For Box M test, we are testing for equality of covariance matrix by groups
# If it is significant, then it means there is no equality of covariance matrices.
#  This data failed the BoxM test at 5% level of sig., but not at 1%. 
#  Let's continue. This shows the subjectivity of statistics!

#test for multivariate normal
library(mvnormtest)
t_admit<-t(admit[,-3])
mshapiro.test(t_admit)
#shapiro-wilk test is not significant at any level.
#Yay! Data is multivariate normal!

# We can set up an outlier test in multivariate sense,
# But we to run a linear regression model and 
# need a proper dependent variable
# Since we have an ordinal categorical variable, we can set that up
admit$decision[admit$De == "notadmit"] <- 1
admit$decision[admit$De == "border"] <- 2
admit$decision[admit$De == "admit"] <- 3
#Now run a linear model
lm_admit <- lm(decision~GPA+GMAT, data=admit)
#Now use the linear model to run an outlier test.
library(car)
outlierTest(lm_admit)
# We see that there is one outlier, but let's use sig. level of 1%. 

#check for VIF
vif(lm_admit)

#run a plot where groups are identified by color
plot(admit$GPA,admit$GMAT,col=admit$De)
#Another cool graph.
library(ggplot2)
qplot(GPA, GMAT, data = admit, colour = De)

# We need the MASS package for the lda function.
install.packages("MASS")
library(MASS)

#run the lda analysis
lda_admit=lda(De~GPA+GMAT,data=admit)
lda_admit
plot(lda_admit)

# we can predict a person with 3.21 gpa and gmat of 497
predictions <- predict(lda_admit,newdata=data.frame(GPA=3.21,GMAT=497))

#then, we can predict each person's admittance
predictions <- predict(lda_admit,admit)
admit2 <- cbind(admit,predictions$class)
View(admit2)

#get a crosstabs table that shows the prior and result
result <- predictions$class
prior <- admit$De
ct <-table(prior,result)

#get a cool drawing of those admitted and not.
install.packages("klaR")
library(klaR)
partimat(De~GPA+GMAT,data=admit,method="lda") 

#what is total correctly guessed?
sum(diag(prop.table(ct)))
#91.8% were guessed accurately.
