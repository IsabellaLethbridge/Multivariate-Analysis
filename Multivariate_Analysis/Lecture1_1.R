# Multivariate Honours

# Chapter 1

# 1.2 Summary Statistics

#Create matrix
(X <- matrix(c(4,1,-1,3,3,5), 3, 2, byrow=TRUE))

#Create 3x1 vector of 1s
(One <- matrix(c(1,1,1), 3, 1, byrow=TRUE))

#Dimensionality
(dims <- dim(X))
(n <- dims[1])
#or read directly from matrix
(p <- ncol(X))

#Calculate mean
(X_bar <- 1/n*t(X) %*% One)

#Save matrix as a data frame
(Xdata <- as.data.frame(X))

#Test from data
apply(Xdata, 2, mean)

#Calculate covariance matrix
#(1)
S <- t(X) %*% X - n*X_bar %*% t(X_bar)
(S <- S/(n - 1))

#(2)
(I3 <- diag(1, 3))

S1 <- t(X) %*% (I3-One %*% t(One)/n) %*% X
(S1 <- S1/(n - 1))

#Test from data
var(Xdata)
cov(Xdata)
apply(Xdata, 2, var)

#Calculate correlation matrix
(D <- diag(diag(S)))
(R <- solve(D)^(1/2) %*% S %*% solve(D)^(1/2))

#Test from data
cor(Xdata)

# 1.3 Graphical Interpretation of a Matrix

#Using deviation vectors
cent_X <- scale(X, center = T, scale = F)
(d1 <- matrix(cent_X[,1]))
(d2 <- matrix(cent_X[,2]))

#s_11 = d1'd1/(n - 1)
S[1,1]
t(d1)%*%d1/(n - 1)

#s_22 = d2'd2/(n - 1)
S[2,2]
t(d2)%*%d2/(n - 1)

#s_12 = d1'd2/(n - 1)
S[1, 2]
t(d1)%*%d2/(n - 1)

