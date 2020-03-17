setwd("/Users/sgriffin/Documents/GitHub/MultivariateStats/MidTerm/")

#### Problem 1 ####

df1 = read.csv("CensusData_PublicandCharters.csv") #reads csv into dataframe
View(df1)

attach(df1)

####1.1:####
average = mean(white, na.rm = TRUE) #mean of percentage white students across all schools (excluding NA's)
print(average)

####1.2:####
i = 10 #i is variable used for iteration 
for (column in df1[,10:15]) #loops through dataframe columns 10-15, calculates the median and prints the median
     {
       median = median(column, na.rm = TRUE)
       cat(colnames(df1[i]),"median is",median, "\n")
       i = i+1 #increment i to iterate through columns 10-15
}

#### 1.3: ####
hist(rating) #plots histogram

#### 1.4: ####
mean(rating, na.rm = TRUE) #calculates mean (excluding NA's)
sd(rating, na.rm = TRUE) #calculates standard deviation (excluding NA's)
median(rating, na.rm = TRUE) #calculates median (excluding NA's)

#### 1.5: ####
cor(rating, frpl, use = "complete.obs") #calculates correlation (complete.obs handles missing values thru casewise deletion)

#### 1.6: ####
#Schools with higher ratings have, on average, higher standardized test scores. While this is only an assumptions, it is likely these schools have resources and the students come from stable backgrounds. If a school has a high percentage of free lunches, it's highly possible that the school is located in a low-income area where students do not have resources and come from troubled backgrounds. Thus, the school with a high percentage of free meals is likely to perform poorer on standardized tests, reducing their rating and explaining the negative correlation. 

#### 1.7: ####
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


#### a.) ####

loop.vector = 3:7 #creates a loop vector of columns 3-7
columns = list("Size","Lot","Bath","Bed","Year") #creates a column name list

n = 1
for (i in loop.vector) { #loops through columns 3-7
  x = df2[,i]#assigns x value to specific column
  plot(x, df2$Price, xlab = columns[n]) #plots x against price with the corresponding x-label
  n = n + 1
} 
  

corr_matrix = cor(df2[c(1:8)], use = "everything") #creates a correlation matrix for all 8 numeric values (columns 1-8)
# install.packages("corrplot") #install package
library(corrplot)
corrplot(corr_matrix, type="upper") #plots the correlation matrix, upper type displays only upper triangular of matrix

#### b.) ####

# install.packages("dummies") #install package for creating dummies; comment out after installing library
library(dummies)
df2.new = dummy.data.frame(df2, names = c("Elem"), sep = "") #dummies columns with names Status and Elem
df2.new = df2.new[-c(1, 9, 18:26)] #deletes redundant columns created by library

lm_model = lm(Price~.,df2.new) #creates regression model
summary(lm_model) #summary of regression model
prediction = predict(lm_model, data.frame(Size = 2500, Lot = 5, Bath = 2, Bed = 4, Year = 1960, Garage = 2, Elemadams=0, Elemcrest = 0, Elemedge = 0, Elemedison = 0, Elemharris = 1, Elemparker = 0)) #prediction using specific parameters
print(prediction) 

# install.packages("car") #comment out after installing library
library(car)

outlierTest(lm_model) #gives observations that are outliers

qqPlot(lm_model, main="QQ Plot") #plots outliers using a Q-Q plot of the residuals

resid_lm_model <- resid(lm_model) #calculates residuals of lm model
hist(resid_lm_model, 
     main="Distribution of Residuals") #creates histogram of residuals

plot(lm_model) # tests for Equal Variance
ncvTest(lm_model)

crPlots(lm_model) # Evaluates linearity

durbinWatsonTest(lm_model) # Test for Autocorrelated Errors (independence)


vif(lm_model) #Evaluates Collinearity

# install.packages("psych") #comment out after installing library
library(psych)
corr_matrix2 = cor(df2[c(2:8)], use = "everything") #creates correlation matrix
####change n-factos report this####
pc_result = principal(corr_matrix2, nfactors=7,rotate="none") #outputs principal components
pc_result 

# install.packages("rela") #comment out after installing library
library(rela)
df_2_numeric = df2[c(2:8)] #new dataframe 
df2_matrix<-as.matrix(df_2_numeric)
df2_paf <-paf(df2_matrix,eigcrit=1,convcrit=.001) # Runs KMO Test
summary(df2_paf)

# install.packages("psych") #comment out after installing library
library(psych)
cortest.bartlett(cor(df_2_numeric),n = 76) #runs Bartlett Test

det(cor(USArrests)) #Finds determinant of correlation matrix


#### Problem 3 ####

df3 = read.csv("birthweight.csv")
View(df3)

#research conducted using https://www.urmc.rochester.edu/encyclopedia/content.aspx?contenttypeid=90&contentid=p02382

#### a.) ####
df3$white[df3$RACE == 1] = 1
df3$white[df3$RACE != 1] = 0
df3$black[df3$RACE == 2] = 1
df3$black[df3$RACE != 2] = 0
df3$other[df3$RACE == 3] = 1
df3$other[df3$RACE != 3] = 0

df3$underweight[df3$LWT < 95] = 1 #https://www.medicalnewstoday.com/articles/321003#other-factors
df3$underweight[df3$LWT >= 95] = 0

df3$frequentvisits[df3$FTV > 2] = 1 
df3$frequentvisits[df3$FTV <= 2] = 0

df3$young_and_black[df3$AGE <= 25 & df3$black == 1] = 1 
df3$young_and_black[df3$AGE > 25 | df3$black != 1] = 0
df3$young_and_white[df3$AGE <= 25 & df3$white == 1] = 1 
df3$young_and_white[df3$AGE > 25 | df3$white != 1] = 0
df3$young_and_other[df3$AGE <= 25 & df3$other == 1] = 1 
df3$young_and_other[df3$AGE > 25 | df3$other != 1] = 0

df3 = df3[-c(4)]

#Run a logistic regression
attach(df3)
weight_prob <- glm(LOW ~., 
            family = "binomial",
            data = df3)
summary(weight_prob)

#interpert the independent variables' coefficients
exp(coef(weight_prob))

#interpret the y's
df3$predicted <- predict(weight_prob) #These are logged values predicted from the logistic regression
df3$weight_prob <- (exp(predict(weight_prob))) / (1+(exp(predict(weight_prob))))
View(df3)

df3$score[df3$predicted >= 0.5] = 1
df3$score[df3$predicted < 0.5] = 0

# Run a second logistic regression 
weight_prob2 <- glm(LOW ~ SMOKE+PTL+underweight+frequentvisits+young_and_black+young_and_other+young_and_white, 
            family = "binomial",
            data = df3)
summary(weight_prob2)

#interpert the independent variables' coefficients
exp(coef(weight_prob2))

#interpret the y's
df3$predicted2 <- predict(weight_prob2) #These are logged values predicted from the logistic regression
df3$weight_prob2 <- (exp(predict(weight_prob2))) / (1+(exp(predict(weight_prob2))))
View(df3)
exp(coef(weight_prob2))
df3$score2[df3$predicted2 >= 0.5] = 1
df3$score2[df3$predicted2 < 0.5] = 0

sum(df3$score-df3$LOW == 0)/189
sum(df3$score2-df3$LOW == 0)/189

#Check Assumptions

#independence (that variables are independently distributed)
#however, we can deduce that each observation is independent of each other given that they are different people.

#multicollinearity
library(car)
vif(weight_prob)
vif(weight_prob2)

#Goodness of fit tests

logLik(weight_prob)
logLik(weight_prob2)
#The larger the absolute value of the log likelihood, the worse the fit. The second model looks better.


#Likelihood Ratio Test
# install.packages("lmtest") #comment out after installing library
library(lmtest)
lrtest(weight_prob,weight_prob2)
#Here, H0 is that smaller model is better.
# If low p-value, then reject that.
# Here, the larger model is better fit.

#Psuedo R-squared (look at McFadden)
library(pscl)
pR2(weight_prob)
pR2(weight_prob2)  


#### b.) ####
newdata = data.frame(AGE=27, LWT=141, SMOKE= 1, PTL= 1, FTV = 3, white = 0, black = 1, other = 0,underweight = 0,frequentvisits=1,young_and_black=0,young_and_other=0,young_and_white=0) #creates new dataframe with all designated parameters
first_mod_pred = predict(weight_prob, newdata, type = "response") #predicts low-weight probability based on first model
second_mod_pred = predict(weight_prob2, newdata, type = "response") #predicts low-weight probability based on second model
first_mod_pred 
second_mod_pred

#### Problem 4 ####

df4 = read.csv("places_rated.csv") #
View(df4)
dim(df4) #329, 10

#### a.) ####
df4_subset <- subset(df4[c(2:4,6:8)])
View(df4_subset)
corr_matrix = cor(df4_subset, use = "everything") #creates a correlation matrix for all 8 numeric values (columns 1-8)
corr_matrix
# install.packages("corrplot") #install package
library(corrplot)
corrplot(corr_matrix, type="upper") #plots the correlation matrix, upper type displays only upper triangular of matrix

# install.packages("psych") #comment out after installing library
library(psych)
cortest.bartlett(corr_matrix,n = 329) #runs Bartlett Test
det(corr_matrix) #Finds determinant of correlation matrix


#standardize variables
scaled_df4 <- apply(df4_subset, 2, scale)


#Find and use covariance matrix
df4.cov <- cov(scaled_df4)

# Find and use eigenvector of the covariance matrix
df4.eigen <- eigen(df4.cov)
# This gives you the eigenvalues and eigenvectors
df4.eigen

# Extract the eigenvectors (also called loadings in PCA)
# We'll just take the first 4 columns
vectors <- df4.eigen$vectors[,1:4]
vectors
vectors <- -vectors
vectors

head(df4_subset)

row.names(vectors) <- c("Climate", "HousingCost", "Healthcare", "Transp", "Educ", "Arts")
colnames(vectors) <- c("PC1", "PC2", "PC3", "PC4")
vectors

PC1 <- as.matrix(scaled_df4) %*% vectors[,1]
PC2 <- as.matrix(scaled_df4) %*% vectors[,2]
PC3 <- as.matrix(scaled_df4) %*% vectors[,3]
PC4 <- as.matrix(scaled_df4) %*% vectors[,4]


# We can then create a data frame with Principal Components scores
PC <- data.frame(City = df4$City, PC1, PC2, PC3, PC4)
head(PC)
View(PC)

remove.packages("ggplot2")
remove.packages("mlr")
install.packages("mlr")
install.packages("ggplot2")
library(ggplot)
# Plot Principal Components for each State
ggplot(PC, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = City), size = 3) +
  xlab("First Principal Component") + 
  ylab("Second Principal Component") + 
  ggtitle("First Two Principal Components of df4 Rated Data")


library(psych)
View(places_rated)

places_rated1 <- places_rated[,-1]

View(places_rated1)

pca_places <- prcomp(places_rated1, scale = TRUE)
pca_places

places_cor <- cor(places_rated1)

scree(places_cor, main = "scree plot")

pca_places_prin2 <- principal(places_rated1, nfactors=4,rotate="none")
pca_places_prin2
fa.diagram(pca_places_prin2)


biplot(pca_places)



























