# Set the working directory where your file is.
setwd("C:/Users/echan1/Downloads/")

#Read the CSV file into R and store it.
colleges <- read.csv("FA_CollegesExample.csv")

colleges1 <- colleges[,-c(1,2)]

View(colleges1)

colleges_cor<-cor(colleges1)

install.packages('psych')
install.packages('GPArotation')

library(psych)
library(GPArotation)
fa_colleges <- fa(colleges1,nfactors = 5,rotate = "varimax",fm="minres")
fa_colleges

print(fa_colleges$loadings,cutoff = 0.3)

fa_colleges_scores <- factor.scores(colleges1, f = fa_colleges, method = "Thurstone")
colleges2 <- cbind(colleges, fa_colleges$scores)
View(colleges2)