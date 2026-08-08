# Multivariate Section B: Canonical Correlation Analysis
# 8 June 2026
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Example 1: CCA from first principles
#-------------------------------------------------------------------------------
# Read in correlation matrix:
rho = matrix(c(1,0.4,0.5,0.6,0.4,1.0,0.3,0.4,0.5,0.3,1.0,0.2,0.6,0.4,0.2,1.0),
             nrow=4,
             ncol=4,
             byrow=T)

# Partition correlation matrix:
rho11 = rho[1:2,1:2]
rho12 = rho[1:2,3:4]
rho21 = rho[3:4,1:2]
rho22 = rho[3:4,3:4]

# Eigen decomposition:
rho11_inv = solve(rho11)
rho22_inv = solve(rho22)

eig11 = eigen(rho11_inv)
rho11_inv_sqrt = eig11$vectors%*%diag(sqrt(eig11$values))%*%t(eig11$vectors)

R = rho11_inv_sqrt%*%rho12%*%rho22_inv%*%rho21%*%rho11_inv_sqrt
eigR = eigen(R)

# Obtain canonical loadings:
a1 = (-1)*rho11_inv_sqrt%*%eigR$vectors[,1]
b1_p = rho22_inv%*%rho21%*%a1

# Calculate covariance of V (scaling):
V1_scaling_factor = (1)/(sqrt(t(b1_p)%*%rho22%*%b1_p)) # scaled 
V1_scaling_factor = matrix(c(V1_scaling_factor,V1_scaling_factor),2,1)
b1 = b1_p*V1_scaling_factor

# Calculate canonical correlation:
rho1 = sqrt(eigR$values[1])

# Canonical coefficients:
Az = t(matrix(a1))
Bz = t(matrix(b1))

# Calculate correlations (canonical variates):
corr_U1Z1 = Az%*%rho11
corr_V1Z2 = Bz%*%rho22

# Proportion of variance exaplined = first eigen value 
# For U1 and V1 only 
prop_var = eigR$values[1]
