# Spring 2020, Problem Set #2

setwd("/users/sgriffin/Documents/GitHub/MultivariateStats/Homework 2/")

### Problem 2 ###

# Two vectors for the dataset
x1 <- c(1,1,1,1,1,1,1,1,1,1,1)
x2 <- c(10,8,13,9,11,14,6,4,12,7,5)
x <- cbind(x1,x2)
x_transposed <- t(x)

xtx <- x_transposed%*%x

#Find inverse of X'X, which is (X'X)^(-1)
xtx_inverse=solve(xtx)
 #9.14,8.14,8.74,8.77,9.26,8.1,6.13,3.1,9.13,7.26,4.74),
#(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.37,8.15,6.42,5.73)
y <- matrix(c(9.14,8.14,8.74,8.77,9.26,8.1,6.13,3.1,9.13,7.26,4.74),
            nrow=11,
            ncol=1)
xty= x_transposed%*%y

#Solve for B Matrix
b <- (xtx_inverse)%*%(x_transposed%*%y)
# B0 = 3.00 and B1 = 0.50. 

### Problem 3 ###

cars <- read.csv("CarSales.csv")

View(cars)

#3.a)

install.packages("mvnormtest")
library(mvnormtest)

cars_2 <- cars[c(1:3,8:10)]
View(cars_2)
cars_t <- t(cars_2)

mshapiro.test(cars_t) 

#3.b)

cars_2 <- cars[c(2:10)]

install.packages("dplyr")
install.packages("ggpubr")
library(dplyr)
library(ggpubr)

attach(cars_2)
ggdensity(cars_2$price, main='Sold', xlab='Sold')
ggdensity(cars_2$price, main='Price', xlab='Price')
ggdensity(cars_2$Seats, main='Seats Density', xlab='Seats')
ggdensity(cars_2$I.Ford.,main='Ford Density', xlab='Ford')
ggdensity(cars_2$I.GM., main='GM Density', xlab='GM')
ggdensity(cars_2$I.Toyota., main='Toyota Density', xlab='Toyota')
ggdensity(cars_2$I.Chrys., main='Chrys Density', xlab='Chrys')
ggdensity(cars_2$rating_happy, main='Happy Rating Density', xlab='Happy Rating')
ggdensity(cars_2$rating_price, main='Price Rating Density', xlab='Price Rating')
ggdensity(cars_2$rating_safety, main='Safety Rating Density', xlab='Safety Rating')

library(ggpubr)

# for (i in cars_2$){
#   print(column)
#   # plot = ggqqplot(cars_2$column)
#   # print(plot)
# }

ggqqplot(cars_2$sold)
ggqqplot(cars_2$price)
ggqqplot(cars_2$Seats)
ggqqplot(cars_2$I.Ford.)
ggqqplot(cars_2$I.GM.)
ggqqplot(cars_2$I.Toyota.)
ggqqplot(cars_2$I.Chrys.)
ggqqplot(cars_2$rating_happy)
ggqqplot(cars_2$rating_price)
ggqqplot(cars_2$rating_safety)

#3.b)
shapiro.test(cars_2$sold)
shapiro.test(cars_2$price)
shapiro.test(cars_2$Seats)
shapiro.test(cars_2$I.Ford.)
shapiro.test(cars_2$I.GM.)
shapiro.test(cars_2$I.Toyota.)
shapiro.test(cars_2$I.Chrys.)
shapiro.test(cars_2$rating_happy)
shapiro.test(cars_2$rating_price)
shapiro.test(cars_2$rating_safety)


#3.e)

car_cov <- cov(cars_2)
car_cor <- cor(cars_2)

det(car_cov)
det(car_cor)

#could not get the following code to work for 3.e) 
#equality across group's covariance matrices
install.packages("biotools", dependencies = TRUE)
library(biotools)
boxM(cars[,-1],cars[,1])

### Problem 4 ###


carsales <- read.csv("CarSales.csv")
View(carsales)

#4.a)
# first model
# soldprice <- lm(sold~price+Seats+rating_happy+rating_price+rating_safety, carsales)
# summary(soldprice)

#4.b.i)
#second model
# soldprice <- lm(sold~., carsales)
# summary(soldprice)

#4.b.ii)
#third model with different data (no singularities)
carsales2 <- carsales[c(1:5,7:10)]
soldprice <- lm(sold~., carsales2)
summary(soldprice)

#4.c)
install.packages("car")
library(car)


outlierTest(soldprice) 


qqPlot(soldprice, main="QQ Plot")

#4.g)
resid_cars <- resid(soldprice) 
hist(resid_cars, 
     main="Distribution of Residuals")

#4.h)
plot(soldprice) 
ncvTest(soldprice)

#4.i)
vif(soldprice)

#4.j)
crPlots(soldprice)

#4.k)
durbinWatsonTest(soldprice)



