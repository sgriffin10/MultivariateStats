setwd("/Users/sgriffin/Documents/GitHub/MultivariateStats/Online_Ass1/")

#### 1 ####

df = read.csv("student_loans.csv") #reads csv into dataframe
View(df)
dim(df)


df$type[df$student == "Yes"] = 1  #dummy for student column
df$type[df$student == "No"] = 0
df$outcome[df$default == "Yes"] = 1 #dummy for dependent variable
df$outcome[df$default == "No"] = 0
df = df[c(-2)] #deletes 2nd column since the assignment says we dont need the variable


#### 2 ####

#Check the assumptions
#Box M Test
install.packages("biotools")
library(biotools)
boxM(df[2:4],df[,1])

#test for multivariate normal
library(mvnormtest)
t_df = df[1:5000,]
t_df = t(t_df[,-1])
mshapiro.test(t_df)

#Linear regression model
lm_df = lm(outcome~balance+income+type, data=df)

#Oulier test
library(car)
outlierTest(lm_df)
#VIF
vif(lm_df)

#Create a list of outliers
outlier_list = list(9539,4160,652,8365,5783,9483,9523,5507,440,4953)

#removes each outlier in list above
for (outlier in outlier_list){
  df = df[-outlier,]
}
dim(df)


#run a plot where groups are identified by color
plot(df$balance,df$income,col=df$default)

library(ggplot2)
attach(df)
qplot(income, balance, data = df, colour = default)

#### 3 ####

install.packages("MASS")
library(MASS)

#Runs LDA analysis
lda_df=lda(default~income+balance+type,data=df)
lda_df
plot(lda_df)

#Predict chance of default
predictions = predict(lda_df,df)
df2 = cbind(df,predictions$class)
# View(df2)

#### 4 ####

#Outputs crosstabs table that shows the prior and result
result = predictions$class
prior = df$default
ct <-table(prior,result)

#Outputs Partition table
install.packages("klaR")
library(klaR)
partimat(default~income+balance+type,data=df,method="lda")

#Accuracy measures
prop.table(ct)
diag(prop.table(ct))
sum(diag(prop.table(ct)))


#### 5 ####

#Log Regression model
attach(df)
log_reg = glm(outcome~balance+income+type, family = "binomial", data = df) 
summary(log_reg)

exp(coef(log_reg)) #outputs independent variables' coefficients

#### 6 ####
df$predicted <- predict(log_reg) #logged values predicted from the logistic regression
df$default_prob <- (exp(predict(log_reg))) / (1+(exp(predict(log_reg))))
# View(df)

#creates score metric variable to measure accuracy 
df$score[df$predicted >= 0.5] = 1 
df$score[df$predicted < 0.5] = 0

#### 7 ####
#accuracy measure divided by n
sum(df$score-df$outcome == 0)/9990




