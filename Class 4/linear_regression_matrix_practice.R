
# Designate the two vectors for the dataset
x1 <- c(1,1,1,1,1)
x2 <- c(49,69,89,99,109)
x <- cbind(x1,x2)
x_transposed <- t(x)

xtx <- x_transposed%*%x

#Find inverse of X'X, which is (X'X)^(-1)
xtx_inverse=solve(xtx)

y <- matrix(c(124,95,71,45,18),
            nrow=5,
            ncol=1)

#Solve for B Matrix
b <- (xtx_inverse)%*%(x_transposed%*%y)

# The B matrix will show us B0 and B1. 