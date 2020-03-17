setwd("/Users/sgriffin/Documents/GitHub/MultivariateStats/MidTerm/")

#### Problem 1 ####

df1 = read.csv("CensusData_PublicandCharters.csv") #reads csv into dataframe
View(df1)

attach(df1)

#Question 1: 0.5631
average = mean(white, na.rm = TRUE) #mean of percentage white students across all schools (excluding NA's)
print(average)

#Question 2: black
i = 10 #i is variable used for iteration 
for (column in df1[,10:15]) #loops through dataframe columns 10-15, calculates the median and prints the median
     {
       median = median(column, na.rm = TRUE)
       cat(colnames(df1[i]),"median is",median, "\n")
       i = i+1 #increment i to iterate through columns 10-15
}

#Question 3: 
hist(rating) #plots histogram

#Question 4: 
mean(rating, na.rm = TRUE) #calculates mean (excluding NA's)
sd(rating, na.rm = TRUE) #calculates standard deviation (excluding NA's)
median(rating, na.rm = TRUE) #calculates median (excluding NA's)

#Question 5: 
cor(rating, frpl, use = "complete.obs") #calculates correlation (complete.obs handles missing values thru casewise deletion)

#Question 6:
#Schools with higher ratings have, on average, higher standardized test scores. While this is only an assumptions, it is likely these schools have resources and the students come from stable backgrounds. If a school has a high percentage of free lunches, it's highly possible that the school is located in a low-income area where students do not have resources and come from troubled backgrounds. Thus, the school with a high percentage of free meals is likely to perform poorer on standardized tests, reducing their rating and explaining the negative correlation. 

#Question 7:
attach(df1) 
NewEngland_states = subset(df1, state == "MA" | state == "RI") #create subset with only schools in MA and RI 
city_subset = subset(NewEngland_states, city == "Springfield" | city == "Providence" | city == "Worcester") #create subset with only schools in Springfield, Providence, and Worcester 

city_list = list("Springfield", "Providence", "Worcester") #creates list of three city names

for (city in city_list) { #loops through each city in list above, calculates mean associated with rating, free lunches, and percentage white students
  city_mean1 = mean(city_subset$rating[city_subset$city == city], na.rm = TRUE)
  city_mean2 = mean(city_subset$frpl[city_subset$city == city], na.rm = TRUE)
  city_mean3 = mean(city_subset$white[city_subset$city == city], na.rm = TRUE)
  cat(city, "rating is", city_mean1, "\n") #prints city mean rating
  cat(city, "free lunch percentage is", city_mean2 * 100, "% \n") #prints city mean free lunch percentage
  cat(city, "white percentage is", city_mean3 * 100, "% \n") #prints city mean white percentage
}

#### Problem 2 ####

df2 = read.csv("realestate.csv") #reads csv into dataframe
View(df2)


#a.) 

loop.vector = 3:7 #creates a loop vector of columns 3-7
columns = list("Size","Lot","Bath","Bed","Year") #creates a column name list

n = 1
for (i in loop.vector) { #loops through columns 3-7
  x = df2[,i]#assigns x value to specific column
  plot(x, df2$Price, xlab = columns[n]) #plots x against price with the corresponding x-label
  n = n + 1
} 
  

corr_matrix = cor(df2[c(1:8)], use = "everything") #creates a correlation matrix for all 8 numeric values (columns 1-8)
install.packages("corrplot") #install package
library(corrplot)
corrplot(corr_matrix, type="upper") #plots the correlation matrix, upper type displays only upper triangular of matrix

#b.)

install.packages("dummies") #install package for creating dummies
library(dummies)
df2.new = dummy.data.frame(df2, names = c("Elem"), sep = "") #dummies columns with names Status and Elem
df2.new = df2.new[-c(1, 9, 18:26)] #deletes redundant columns created by library

lm_model = lm(Price~.,df2.new) #creates regression model
summary(lm_model) #summary of regression model
prediction = predict(lm_model, data.frame(Size = 2500, Lot = 5, Bath = 2, Bed = 4, Year = 1960, Garage = 2, Elemcrest = 0, Elemedge = 0, Elemedison = 0, Elemharris = 1, Elemparker = 0)) #prediction using specific parameters
print(prediction) #prints predicted price value of $127,496


# A second useful function to use is principal from the "psych" package
# This one actually gives us more useful information

install.packages("psych")
library(psych)
corr_matrix2 = cor(df2[c(2:8)], use = "everything")
pc_result = principal(corr_matrix2, nfactors=4,rotate="none") #nfactors should be set to (at most) n-1
pc_result 
#Notice that principal gives us proportion of variance explained and cumulative proportion explained! This is useful



# Now, we should check our assumptions
# Run KMO Test
install.packages("rela")
library(rela)
df_2_numeric = df2[c(2:8)]
df2_matrix<-as.matrix(df_2_numeric)
df2_paf <-paf(df2_matrix,eigcrit=1,convcrit=.001)
summary(df2_paf)
# OUr KMO ranges from 0 to 1
# Here, it is 0.65. Again, no rule of thumb on KMO, but should be reported nonetheless.
# We have 50 observations for 5 variables = not nearly the rule of thumb of 20:1
# However, this is a subjective assumption. Let's continue.


#Bartlett Test
install.packages("psych")
library(psych)
cortest.bartlett(cor(df_2_numeric),n = 50)
#Bartlett test is significant. We can move on.
cortest.bartlett(cov(df_2_numeric),n = 50)
# IF we do the test using covariance matrix, what happens?
# We cannot move on! This is due to the scales between variables being massively different.
# It is best to use the correlation matrix for the PCA when asked.
# Alternatively, this just signals to us that we need to scale the variables prior to PCA.


#Find determinant of correlation matrix
det(cor(USArrests))
#This is positive. Good.
# We can also check covariance matrix. Will be same conclusion, different numbers.
det(cov(USArrests))

#### Problem 3 ####

df3 = read.csv("birthweight.csv")
view(df3)













