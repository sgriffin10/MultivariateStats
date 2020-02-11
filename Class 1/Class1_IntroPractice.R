# In-Class Review
# Practice R

# mean comment...nothing after # runs as code.
# If you want to run code, just click on the "Run" button.
#   Hint: You can highlight code and just click Run for that portion.

# Let's import the CSV file
# Set the working directory first (different from yours)
# Set the working directory where your file is.
getwd()
setwd("/Users/sgriffin/Documents/Github/MultiStats")
# Notice I had to switch from backkslash to forwardslash
# Now, read the file data into R
salesdata <- read.csv("Class1_PubRepExample.CSV")
#Now, the data is stored as "salesdata"

View(salesdata$Copies)  # allows us to view the data set
# x = salesdata$Copies
        # Notice that R is quite case-sensitive, need cap V
names(salesdata)  # shows us the names of the variables 
head(salesdata)# this allows me to check out the first data and variable names

# It makes things easier in R if we define variables from datasets
# Unlike Minitab, we can't just tell it the variable name
# Each time we refer to a variable from a dataset, it needs both
#    the dataset name and variable, in the form "dataset$variablename"
# let's say we want to call "Copies" as x and "Revenue" as y

attach(salesdata)
x <- Copies
y <- Revenues
# Once we run this code, we can refer to Copies as x and Revenues as y!
# In general, if we want to store data or dataset in any variable name, we use "variable = data/dataset"

# Now, we can use x and y!


################################################################

# EXAMPLES OF UNIVARIATE ANALYSES

# What is the MEAN of copies? Two ways to do this:
mean(salesdata$Copies)  #this is what you must do if you did not define x and y
mean(x) #this is the easier way!
mean(Revenues)
mean(salesdata$Revenues)
mean(y)
# MEDIAN
median(x)
median(y)
# standard deviation
sd(x)
sd(y)
# SUMMARY
summary(x)
summary(y)
# DESCRIBE would give us some descriptive statistics
# But we need to install an R package first
# In general we install an R package using the function "install.packages("")", such as
# install.packages("Hmisc")  # Here, we are installing the package Hmisc; make sure it's in quotes
#Then, we must load the package. One annoying thing is that each time we restart RStudio,
#   we must reload a package that we want to use. We reload using the function "library()"
library(Hmisc)
# Now, we can use the Hmisc package.
describe(x)
# Histogram
hist(x,main = "Histogram of copies", xlab = "Free copies given by each!")
#boxplot
boxplot(y,
        main = "Boxplot of revenue",
        ylab = "Revenue by each seller in dollars",
        col = "blue")

# others to try: min(), max(),fivenum(), and many others!
#################################################################

# EXAMPLES OF BIVARIATE ANALYSES

# correlation between two variables
cor(x,y)

# Let's visualize the data in a scatterplot
plot(x, y, main="Copies (x) vs. Revenues (y)", 
     xlab="Copies", ylab="Revenues")
# you can add a line to it too!
abline(lm(y~x), col="red") # regression line (y~x) 

# regression
# lm(response ~ predictor) will give us the coefficient and intercept of the regression line, such as
lm(y~x)
# we can store this regression
reg_sales = lm(y~x)
# then summarize it
summary(reg_sales)
#or, you didn't have to store the regression, but instead do this in one command.
summary(lm(y~x))


##############################################################

# MULTIPLE DIMENSIONS, but still UNIVARIATE technically.
# What happens when there are three variables?!?!?!

# We must install the scatterplot3d package
# install.packages("scatterplot3d")
# load the package
library(scatterplot3d)
# Let's add another variable, Z, since we have data on calls made by salesperson!
# This will serve as our third variable of interest.
z <- salesdata$Calls
# Draw the scatterplot in 3D!
scatterplot3d(x,y,z, main="Sales!")

# A cooler way of doing this is using the package rgl
# rgl will show graphics in an external interactive window that you can drag and play with.
# install and load the package
# install.packages("rgl")
library(rgl)
# to use rgl, we have to open an RGL device
rgl.open() # Open a new RGL device
plot3d(x, y, z,col="red", size=3) # Scatter plot

# Another cool package: Rcmdr
# install.packages("Rcmdr")
library(Rcmdr) #load
scatter3d(x,y,z)# scatter plot
# What happens when you add another variable? What kind of scatterplot do you get?
# A lot of this class will be about thinking about beyond 2-dimensions analytically.


