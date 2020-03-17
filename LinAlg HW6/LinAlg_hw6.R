 setwd("/Users/sgriffin/Documents/GitHub/MultivariateStats/LinAlg HW6/")
 
 df = read.csv("Problems27-28Data.csv")
 View(df)
 df2 = df[-c(48),] #delete row with outlier
 View(df2)


 #### Problem 26 ####
 
 attach(df2)
 score_regression_0 = lm(Final~., df2, subset = Section == 0)
 score_regression_1 = lm(Final~., df2, subset = Section == 1)
 score_regression_both = lm(Final~., df2)
 score_regression_list = list(score_regression_0,score_regression_1,score_regression_both)
 color_list = list("red","green","blue")
 scatterplot = plot(HWGrade,Final,main="HW Grade vs. Final Score", xlab="HW Grade", ylab="Final Score")
 i=1
 for (regression in score_regression_list)
 {
        abline(regression, col=color_list[[i]])
        i = i+ 1
        print(regression)

 }
 
 twentysix_b = predict(score_regression, data.frame(HWGrade = 27, Section = 0))
 print(twentysix_b)
 
 
 ##### Problem 27 #####
 
 # Designate the two vectors for the dataset
 x1 <- c(rep(1,54))
 x2 <- c(HWGrade)
 x3 <- c(Section)
 x <- cbind(x1,x2,x3)
 x
 x_transposed <- t(x)
 
 xtx <- x_transposed%*%x
 
 #Find inverse of X'X, which is (X'X)^(-1)
 xtx_inverse=solve(xtx)
 
 y <- matrix(c(Final))
 y
 
 #Solve for B Matrix
 b <- (xtx_inverse)%*%(x_transposed%*%y)
 b
 
 
std_10 =  df2[10,]
predicted_equation = b[1] + b[2]*std_10[,1] + b[3]*std_10[,2]
print(predicted_equation)


 


 
 
 
 
 