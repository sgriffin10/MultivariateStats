#Libary
library(Hmisc)
library(dplyr)
library(lattice)
library(ggplot2)
library(ggvis)

setwd("Documents/Github/")

#Load Data
Final = read.csv("Dataset.csv")
is.data.frame(Final)

#Detect Missing Values
anyNA(Final)
summary(Final)

#Handling Missing Values
for (i in 1:ncol(Final)) #loops through all columns
{
  Final[,i][is.na(Final[,i])] = mean(Final[,i][!is.na(Final[,i])]) #replaces all NAs with mean
}

anyNA(Final)

#library needed for correlation plot
library(GGally)

#ggcorr is function, first argument is columns, second argument is method, and third argument is number of breaks for correlation
ggcorr(Final[c(6:17,44:51)],method = c("everything","pearson"),nbreaks=6)


#understand Data
dim(Final)
str(Final)
attributes(Final)
names(Final)
class(Final)
length(Final)
