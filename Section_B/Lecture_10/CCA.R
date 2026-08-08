# Multivariate Analysis Section B: Canonical Correlation Analysis
# 10 June 2026
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Question 1
#-------------------------------------------------------------------------------
# Setup
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("DataTidy23RodoHonsMult", quietly = TRUE)) {
  # note that you'll need to reinstall the package
  # to get the latest version (as of 24 April 2024)
  # with the data set
  remotes::install_github("MiguelRodo/DataTidy23RodoHonsMult@2024")
}
data("corr_mat_profit", package = "DataTidy23RodoHonsMult")
corr_mat_profit

# a
# Partition correlation matrix: 
sigma11 = corr_mat_profit[1:6,1:6]  
sigma12 = corr_mat_profit[1:6,7:8]  
sigma21 = corr_mat_profit[7:8,1:6]
sigma22 = corr_mat_profit[7:8,7:8]

# Eigen decomposition:
sigma11_inv = solve(sigma11)
eig11 = eigen(sigma11_inv)
sqrt_inv_sigma11 = eig11$vectors%*%diag(sqrt(eig11$values))%*%t(eig11$vectors)

sigma22_inv = solve(sigma22)
eig22 = eigen(sigma22_inv)
sqrt_inv_sigma22 = eig22$vectors%*%diag(sqrt(eig22$values))%*%t(eig22$vectors)

R = sqrt_inv_sigma11%*%sigma12%*%sigma22_inv%*%sigma21%*%sqrt_inv_sigma11
eigR = eigen(R)

a1 = t(eigR$vectors[,1])%*%sqrt_inv_sigma11
a1 = -a1
round(a1,3)

# b
# Calculate b1 (proportional):
b1_p = sigma22_inv%*%sigma21%*%t(a1)
b1_p

# Calculate scaling factor:
sf = 1/sqrt(t(b1_p)%*%sigma22%*%b1_p)

# Calculate b1:
b1 = b1_p%*%sf
b1

# c
# Find first canonical correlation coefficient:
rho1 = sqrt(eigR$values[1])
rho1

# d
# As variables 1,4,6 increase, U1 increases.
# V1 represents the difference between Q and REV.

# e
# Store coefficients as matrices:
Az = t(matrix(a1))
Bz = t(matrix(b1))


# Calculate correlations between canonical variables and measured variables:
corr_U1Z1 = Az%*%sigma11
corr_V1Z2 = Bz%*%sigma22
corr_U1Z1
corr_V1Z2

# All coefficients are negative for U1, with variables 1,4,6 loading most
# strongly.
# Interpretation is based on magnitude. 
# Both coefficients for V1 load strongly, again in the same direction. V1
# represents overall market-value profitability. 

#-------------------------------------------------------------------------------
# Question 2
#-------------------------------------------------------------------------------
# a
# CCA aims to maximise correlation between sets of linear combinations of 
# variables. Although the variances may be larger for X11 and X22, they have 
# 0 covariance. Variables X21 and X12 have large covariance (0.95) which 
# increases correlation.
# correlation = covariance/sqrt(var1)*sqrt(var2) 

# b
# There will be no correlation between the first canonical variables.
# *look at formulas in slides. 

# c
# Increases the standard devation by a factor of 1/sqrt(2). The denominator 
# becomes larger which covariance stays the same, thus the correlation 
# decreases. 

# d
# If the variance were to double, the standard deviation would increase by a 
# factor of 1/sqrt(2). In order for the variances to have unit variances the 
# lengths of the first canonical coefficient vectors would decrease. 

#-------------------------------------------------------------------------------
# Question 3
#-------------------------------------------------------------------------------
# Canonical coefficients are not scale invariant. Canonical correlations are. 