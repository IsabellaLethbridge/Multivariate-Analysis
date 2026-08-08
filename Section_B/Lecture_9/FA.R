# Multivariate Section B: Factor Analysis
# 25 May 2026
# Isabella Lethbridge

#-------------------------------------------------------------------------------
# Question 1
#-------------------------------------------------------------------------------
# Loading vector:
L <- matrix(c(0.9,0.7,0.5),
            nrow=3,
            ncol=1,
            byrow=TRUE)

# Covariance matrix (errors):
Sigma <- matrix(c(0.19,0,0,0,0.51,0,0,0,0.75),
                nrow=3,
                ncol=3,
                byrow=TRUE)

# Generate covariance matrix:
cov_mat <- L%*%t(L)+Sigma

#-------------------------------------------------------------------------------
# Question 2
#-------------------------------------------------------------------------------
dat <- read.csv("Section_B/Lecture_9/Air Pollution Data .csv")

# (a)
# Generate a sample correlati0n matrix:
cor_mat <- cor(dat)
round(cor_mat,2)

# (b)
# Perform eigen decomposition of correlation matrix:
eig_cor <- eigen(cor_mat)
eig_val <- eig_cor$values
eig_vec <- eig_cor$vectors

# Obtain loadings:
l1 <- eig_vec[,1:1]*sqrt(eig_val[1])
l1
l2 <- eig_vec[,1:2]%*%diag(sqrt(eig_val[1:2]))
l2

# Commisibilities:
h1 <- diag(l1%*%t(l1))
h1
h2 <- diag(l2%*%t(l2))
h2

# Calculate psi:
psi1 <- 1-h1
psi2 <- 1-h2

# Calculate proportion of variance explained:
prop_var1 <- eig_val[1]/7
prop_var2 <- eig_val[1]/7 + eig_val[2]/7

# (c)
FA2 <- factanal(dat,2,rotation="none")
print(FA2,cutoff=0)

l3 <- FA2$loadings

# Calculate communality:
h3 <- diag(l3%*%t(l3))

# Calculate psi:
psi3 <- 1-h3
# or
FA2$uniquenesses

# Variance explained:
FA2$loadings

# (d)
# The loadings differ due to the different estimation approaches.
# PC method explains more variance (designed to maximise variance)
# ML method produces a more interpretable structure

# (e)
FA_r <- factanal(dat,factors=2,rotation="varimax")
print(FA_r,cutoff=0)

# Factor 1:
# CO, NO, NO2 (load strongly)
# HC = 0.251
# Atmospheric gases (excluding ozone) 

# Factor 2:
# O3 (ozone = dominated factor): atmospheric pollution
# Allows one to easily identify the impact of ozone concentration 

# Can distinguish between different gases.

# (f)
# Calculate factor scores:
p <- ncol(dat)
L <- as.matrix(FA2$loadings[1:p,1:2])
PSI <- diag(FA2$uniquenesses)
PSI_inv <- solve(PSI)

x <- as.matrix(dat)
mu <- colMeans(x)
x_std <- scale(x,center=TRUE,scale=TRUE)

A <- solve(t(L)%*%PSI_inv%*%L)%*%t(L)%*%PSI_inv
F_hat <- x_std%*%t(A)
