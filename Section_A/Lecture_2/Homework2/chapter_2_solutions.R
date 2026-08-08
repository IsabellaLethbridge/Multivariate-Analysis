### Homework exercise 2.1

A <- matrix(c(4, 3, 8, 6, 8, -9), nrow = 2)

t(A) %*% A
eigen(t(A) %*% A)

A %*% t(A)

#Creating the decomposition from scratch
(U <- eigen(A %*% t(A))$vectors)
(D <- diag(sqrt(eigen(A %*% t(A))$values)))
(V <- zapsmall(eigen(t(A) %*% A)$vectors))

#Note that in the question V was defined as
V[, 1] <- -V[, 1]
V

#The signs of the vectors of U must be changed such that the decomposition holds
#We can see that both vectors' signs need to change:
U <- -U

#Checking the result
U %*% D %*% t(V[,-3])

U


#Starting with the svd function:
(svd_func <- svd(A))

#Here it is v_2 that has a different sign, so we have to change that of u_2 as well
svd_func$v[, 1] <- -svd_func$v[, 1]
svd_func$u[, 1] <- -svd_func$u[, 1]

#Checking the result again
svd_func$u %*% diag(svd_func$d) %*% t(svd_func$v)

svd_func$u
