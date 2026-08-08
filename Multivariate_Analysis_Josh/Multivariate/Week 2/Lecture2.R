# Multivariate Honours

# Chapter 2

#2.2.3 Singular Value Decomposition

#Create matrix from class example
A <- matrix(c(3, -1, 1, 3, 1, 1), nrow = 2)

#Creating the decomposition from scratch
(U <- eigen(A %*% t(A))$vectors)
(D <- diag(sqrt(eigen(A %*% t(A))$values)))
(V <- zapsmall(eigen(t(A) %*% A)$vectors))

#Note that here we need the first position in each eigenvector to have the same sign
U[,2] <- -U[,2]
V[,1] <- -V[,1]

#Checking the result
U %*% cbind(D, 0) %*% t(V)


#Using the SVD function
(svd_func <- svd(A))

#Checking the result again
svd_func$u %*% diag(svd_func$d) %*% t(svd_func$v)

A = matrix(c(4, 3, 8, 6, 8, -9), ncol = 3)
A

vals = zapsmall(eigen(t(A) %*% A)$values)
vals

U = round(eigen(A %*% t(A))$vectors, 3)
D = zapsmall(diag(sqrt(eigen(t(A) %*% A)$values)))
V = zapsmall(eigen(t(A) %*% A)$vectors)
U ; D; V

svd = svd(A)
svd


