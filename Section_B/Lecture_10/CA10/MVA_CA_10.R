# Multivariate Analysis Assignment 10
# 10 May 2026
# Isabella Lethbridge 

# Setup
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("DataTidy23RodoHonsMult", quietly = TRUE)) {
  # note that you'll need to reinstall the package
  # to get the latest version (as of 24 April 2024)
  # with the data set
  remotes::install_github("MiguelRodo/DataTidy23RodoHonsMult@2024")
}
data("corr_mat_profit", package = "DataTidy23RodoHonsMult")
# corr_mat_profit

# Question 1
# (a) 
# Partition sample correlation matrix:
rho   <- corr_mat_profit
rho11 <- rho[1:6,1:6]
rho12 <- rho[1:6,7:8]
rho21 <- rho[7:8,1:6]
rho22 <- rho[7:8,7:8]

rho11_inv      <- solve(rho11)
eig11          <- eigen(rho11_inv)
sqrt_rho11_inv <- eig11$vectors%*%diag(sqrt(eig11$values))%*%t(eig11$vectors)

rho22_inv      <- solve(rho22)
eig22          <- eigen(rho22_inv)
sqrt_rho22_inv <- eig22$vectors%*%diag(sqrt(eig22$values))%*%t(eig22$vectors)

RHO <- sqrt_rho11_inv%*%rho12%*%rho22_inv%*%rho21%*%sqrt_rho11_inv

# Eigen decomposition:
eigRHO <- eigen(RHO)

# Obtain the canonical loadings: 
a_1 <- sqrt_rho11_inv%*%eigRHO$vectors[,1]
a_1 <- (-1)*a_1

# (b) 
b_1p <- rho22_inv%*%rho21%*%a_1

# Standardise to unit variance: 
V1_var <- 1/(sqrt(t(b_1p)%*%rho22%*%b_1p))
V1_var <- matrix(c(V1_var, V1_var), 2, 1)
b_1    <- b_1p*(V1_var)

# (c)
rho_1 <- sqrt(eigRHO$values[1])

# (d)

# (e)
A_z <- t(matrix(a_1))
B_z <- t(matrix(b_1))

corr_U1Z1 <- A_z%*%rho11
corr_V1Z2 <- B_z%*%rho22
