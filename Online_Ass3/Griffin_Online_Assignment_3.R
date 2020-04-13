setwd("/Users/sgriffin/Documents/GitHub/MultivariateStats/Online_Ass3/")

#### Dataset Basics ####

df = read.csv("votes.csv")
View(df)
dim(df)
levels(df$method)

#### PART 1 ####

install.packages("MASS")      
install.packages("car")       
install.packages("carData")
install.packages("pastecs")   

library(MASS)
library(car)
library(pastecs)

#### Assumptions ####

install.packages("mvnormtest")		
library(mvnormtest)				
depvar = data.frame(df[,2:3])
df_t = t(depvar)	
shapiro.test(df_t)

#Test for univariate normality.
install.packages("nortest")		    
library(nortest)			    
depvar = data.frame(df[,2:3])	
names(depvar) = c("Dis1","Dis2") 
attach(depvar)			   

ad.test(Dis1)		# Anderson-Darling
cvm.test(Dis1)	# Cramer-von Mises
lillie.test(Dis1)	# Kolmogorov-Smirnov
pearson.test(Dis1)	# Pearson chi-square
sf.test(Dis1)		# Shapiro-Francia

ad.test(Dis2)		# Anderson-Darling
cvm.test(Dis2)	# Cramer-von Mises
lillie.test(Dis2)	# Kolmogorov-Smirnov
pearson.test(Dis2)	# Pearson chi-square
sf.test(Dis2)		# Shapiro-Francia

#Now, we have to check for equal covariance matrices between groups.
library(biotools)
boxM(df[,3:4],df[,1]) 

installed.packages("psych")
library(MASS)
library(car)
library(psych)

#### MANOVA ####

grp = factor(df[,1]) 
Y = cbind(df$district1vote,df$district2vote)
fit = manova(Y~grp)

summary(fit,test="Wilks")
summary(fit,test="Hotelling-Lawley")
summary(fit,test="Pillai")
summary(fit,test="Roy")

##### ANOVA ####
summary.aov(fit)

#### Variable Means ####
anovadata <- data.frame(df)
names(anovadata) <- c("group", "Dis1","Dis2","method","perc_repub")
anovadata
describeBy(anovadata,anovadata$method)



#### PART 2 ####

install.packages("effects")	

# Load packages

library(MASS)
library(car)
library(psych)

#### Two-Way MANCOVA ####
# options(scipen=999
attach(df)
model = manova(Y ~method + perc_republicans, data = df)

summary(model,test = "Wilks")

#### Two-Way MANOVA ####
model = manova(Y ~ perc_republicans, data = df)
summary(model,test = "Wilks")
model1 = manova(Y ~ method, data = df)
summary(model1,test = "Wilks")

#### Variable Means ####

library(psych) #redundant
describeBy(df,df$method)

#### ANCOVA ####

modelA = aov(district1vote ~ method + perc_republicans, data = df)
summary(modelA)

modelB = aov(district2vote ~ method + perc_republicans, data = df)
summary(modelB)

library(effects)
adjmeanA = effect("method",modelA)
summary(adjmeanA)

adjmeanB = effect("method",modelB,xlevels=4)
summary(adjmeanB)

