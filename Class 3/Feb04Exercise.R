#Practice
# A1 = c(1,1,1,1,1,1)
# A2 = c(10,8,13,9,11,14)
A <- matrix(c(1,1,1,1,1,1,10,8,13,9,11,14),          
            nrow=6,
            ncol=2,
            )
A

B <- matrix(c(8,7,8,9,8,10),
            nrow=6,
            ncol=1,
            byrow=TRUE)
B

transposedA = t(A)

firstvar = transposedA%*%A

inversevar = solve(firstvar)

secondvar = transposedA%*%B


A_matrix =inversevar%*%secondvar