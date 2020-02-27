
data("USArrests")
head(USArrests, 10)

#It is usually beneficial for each variable to be centered at zero for PCA
# Then, we can always use covariance matrix!

#standardize variables
# apply(data, 1=row or 2=column, scale function which normalizes data to 0)
scaled_df <- apply(USArrests, 2, scale)

#Find and use covariance matrix
arrests.cov <- cov(scaled_df)

# Find and use eigenvector of the covariance matrix
arrests.eigen <- eigen(arrests.cov)
# This gives you the eigenvalues and eigenvectors
arrests.eigen

# Extract the eigenvectors (also called loadings in PCA)
# We'll just take the first 4 columns
vectors <- arrests.eigen$vectors[,1:4]

# When Eigenvectors are calculated, they might be positive or negative. 
# we can switch the signs to point in differen direction, but we must switch all the signs. 
# By default, eigenvectors in R start pointing in the negative direction. 
#For this example, we'd prefer the eigenvectors point in the positive direction because it leads 
# to more logical interpretation of graphical results later. 
# To use the positive-pointing vector, we multiply the default loadings by -1. 
vectors <- -vectors
row.names(vectors) <- c("Murder", "Assault", "UrbanPop", "Rape")
colnames(vectors) <- c("PC1", "PC2", "PC3", "PC4")
vectors
#By examining the principal component vectors above, 
# we can infer the the first principal component (PC1) roughly corresponds to an overall rate of serious crimes 
# since Murder, Assault, and Rape have the largest values. 

#The second component (PC2) is affected by UrbanPop more than the other three variables, 
# so it roughly corresponds to the level of urbanization of the state, with some opposite, smaller influence by murder rate.

#If we project the raw data points onto the first eigenvector,
#the projected values are called the principal component scores for each observation.
# Calculate Principal Components scores
PC1 <- as.matrix(scaled_df) %*% vectors[,1]
PC2 <- as.matrix(scaled_df) %*% vectors[,2]
PC3 <- as.matrix(scaled_df) %*% vectors[,3]
PC4 <- as.matrix(scaled_df) %*% vectors[,4]


# We can then create a data frame with Principal Components scores
PC <- data.frame(State = row.names(USArrests), PC1, PC2, PC3, PC4)
head(PC)

# Now that we've calculated the first and second principal components for each US state, 
# we can plot them against each other and produce a two-dimensional view of the data. 
# The first principal component (x-axis) roughly corresponds to the rate of serious crimes. 
# States such as California, Florida, and Nevada have high rates of serious crimes, 
# while states such as North Dakota and Vermont have far lower rates. 
# The second component (y-axis) is roughly explained as urbanization, 
# which implies that states such as Hawaii and California are highly urbanized, 
# while Mississippi and the Carolinas are far less so. A state close to the origin, 
# such as Indiana or Virginia, is close to average in both categories.

install.packages("ggplot2")
library(ggplot2)
# Plot Principal Components for each State
ggplot(PC, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = State), size = 3) +
  xlab("First Principal Component") + 
  ylab("Second Principal Component") + 
  ggtitle("First Two Principal Components of USArrests Data")

# Because PCA is unsupervised, this analysis on its own is not making predictions about crime rates, 
# but simply making connections between observations using fewer measurements.




#There are also built in Principal Components Functions in R

pca_result <- prcomp(USArrests, scale = TRUE)
#Here, we can see what information R stores from the dataset as a result of prcomp
names(pca_result)
#"center" are actually the means and "scale" is actually standard deviations. This is useful.
pca_result$center
pca_result$scale

#Also, "rotation" is where the principal components scores are stored
pca_result$rotation

#The "rotation" signs can be changed to have it similar to what we had before!
pca_result$rotation <- -pca_result$rotation
pca_result

#The biplot is the most useful for determining correlation
biplot(pca_result, scale = 0) # scale = 0 ensures that data is scaled


# A second useful function to use is principal from the "psych" package
# This one actually gives us more useful information
install.packages("psych")
library(psych)
pc_result2 <- principal(cov(scaled_df),nfactors=4,rotate="none") #nfactors should be set to (at most) n-1
pc_result2 
#Notice that principal gives us proportion of variance explained and cumulative proportion explained! This is useful



# Now, we should check our assumptions


# Run KMO Test
install.packages("rela")
library(rela)
USArrests_matrix<-as.matrix(USArrests)
USArrests_paf <-paf(USArrests_matrix,eigcrit=1,convcrit=.001)
summary(USArrests_paf)
# OUr KMO ranges from 0 to 1
# Here, it is 0.65. Again, no rule of thumb on KMO, but should be reported nonetheless.
# We have 50 observations for 5 variables = not nearly the rule of thumb of 20:1
# However, this is a subjective assumption. Let's continue.


#Bartlett Test
install.packages("psych")
library(psych)
cortest.bartlett(cor(USArrests),n = 50)
#Bartlett test is significant. We can move on.
cortest.bartlett(cov(USArrests),n = 50)
# IF we do the test using covariance matrix, what happens?
# We cannot move on! This is due to the scales between variables being massively different.
# It is best to use the correlation matrix for the PCA when asked.
# Alternatively, this just signals to us that we need to scale the variables prior to PCA.


#Find determinant of correlation matrix
det(cor(USArrests))
#This is positive. Good.
# We can also check covariance matrix. Will be same conclusion, different numbers.
det(cov(USArrests))





