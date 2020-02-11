#Matrix Basics in R

#build a vector
#let's build a 1x5 vector and call it V

V <- c(1,0,4,9,10)
V
V <- c(1:10)
V
# build a matrix
#Let's build a matrix and call it M
M <- matrix(c(1,0,7,1),
            nrow=2,
            ncol=2)
M
#Notice that this builds a 2 row by 2 column matrix
# also notice that by default the numbers go downward by column
#If you would like to have it go by row, then do the following:

M <- matrix(c(1,0,7,1),
            nrow=2,
            ncol=2,
            byrow=TRUE)
M

# We may also build this matrix by binding multiple vectors
A <- c(1,0, 3, 4)
B <- c(7,1)
C <- rbind(A,B) #row bind
D <- cbind(A, B) #column bind

#build an identity matrix
id_3 <- diag(3)
  
  
# YOu may also build a matrix with the diagonal command
# This helps if there's only numbers on the diagonals
# Let's call this matrix "E"
E <- diag(nrow=2,
          ncol=2)
#By default this includes 1's on the diagonals
# If you would like them not to be 1's, do the following:
F <- diag(c(2,3), nrow = 4, ncol = 4)
#Here we build a 4x4 matrix,
# What do you notice about the diagonals?

#CORE ALGEBRA OPERATIONS

#Addition
#Let's add M to M
M+M
#Let's add M to D; what happens?
M+D

#Multiply by Scalar and store in N
N <- 2*M

#Multiple N by M
M%*%M #multiply two matrices together
#Is this correct?
#Notice that the * operator does not do matrix multiplication


#Transpose Matrix
#We can transpose with the t() function
t(M) 

# Find the Inverse
solve(M)

#Find the Determinant
det(M)

#number of rows and columns in a matrix
nrow(M)
ncol(M)
dim(M)

#Show certain element
#Show the first row, second column
# NameofMatrix[row,column]
M[1,2]
M[2,]
M[,2]

###########

#Practice
A <- matrix(c(3,1,5,1,6,2),          
            nrow=2,
            ncol=3,
            byrow=TRUE)

B <- matrix(c(3,4,1,-1,-2,0),
            nrow=3,
            ncol=2,
            byrow=TRUE)

C <- matrix(c(0,0,1,1,0,0,0,1,0),
            nrow=3,
            ncol=3,
            byrow=TRUE)

A+t(B)

A%*%C

A%*%B

solve((A%*%B))

solve(C)

B%*%B
# This is not possible because the number of columns
# in the first matrix is not the same as numebr of rows
# in the second matrix