#Problem 1. e,f,g,h,i,j,k

#A Matrix
first_row <- c(3,1,5)
second_row <- c(1,6,2)
A <- rbind(first_row,second_row) #row bind

#B Matrix
first_col <- c(3,3,4)
second_col <- c(5,4,3)
B <- cbind(first_col, second_col)

#C Matrix
first_column <- c(0,1,8)
second_column <- c(2,2,5)
third_column <- c(2,7,0)
C <- cbind(first_column, second_column, third_column)

transposedB = t(B)

#1.e)
one_e = A + transposedB

#1.f)
one_f = A%*%C

#1.g)
one_g = B%*%C
#error: non-conformable arguments therefore impossible

#1.h)
one_h = B%*%transposedB

#1.i)
one_i = solve(one_f) #error: 'a' (2x3) must be square

#1.j)
one_j = det(A) #error: A not a square matrix

#1.k)
one_k = det(C)
