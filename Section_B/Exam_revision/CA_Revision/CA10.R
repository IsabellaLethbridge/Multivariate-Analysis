# Multivariate Analysis Section B: Canonical Correlation Analysis 
# 16 July 2026 
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Setup
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("DataTidy23RodoHonsMult", quietly = TRUE)) {
  # note that you'll need to reinstall the package
  # to get the latest version (as of 24 April 2024)
  # with the data set
  remotes::install_github("MiguelRodo/DataTidy23RodoHonsMult@2024")
}
data("corr_mat_profit", 
     package = "DataTidy23RodoHonsMult",
     )
View(corr_mat_profit)

#-------------------------------------------------------------------------------
# Question 1
# (a)
# Find estimated coefficient vector: 
# Partition correlation matrix:
S11 = as.matrix(corr_mat_profit[1:6,1:6])
S12 = as.matrix(corr_mat_profit[1:6,7:8])
S21 = as.matrix(corr_mat_profit[7:8,1:6])
S22 = as.matrix(corr_mat_profit[7:8,7:8])

# Calculate inverse:
S11_inv = solve(S11)
S22_inv = solve(S22)

# Calculate sqrt:
S11_eig = eigen(S11_inv)
S11_inv_sqrt = S11_eig$vectors%*%diag(sqrt(S11_eig$values))%*%t(S11_eig$vectors)

S22_eig = eigen(S22_inv)
S22_inv_sqrt = S22_eig$vectors%*%diag(sqrt(S22_eig$values))%*%t(S22_eig$vectors)

# Calculate R:
R = S11_inv_sqrt%*%S12%*%S22_inv%*%S21%*%S11_inv_sqrt

# Determine first canonical variate a: 
R_eig = eigen(R)
a1 = -1*t(R_eig$vectors[,1])%*%S11_inv_sqrt
round(t(a1),4)

# (b) 
# Determine first canonical variate b:
b1_prop = S22_inv%*%S21%*%t(a1) 

# Scale b1_prop:
b1_sf = 1/(sqrt(t(b1_prop)%*%S22%*%b1_prop))

# Obtain b1:
b1 = b1_prop%*%b1_sf
round(t(b1),4)

# (c)
# Find first canonical correlation coefficient:
round(sqrt(R_eig$values[1]),4)

# (d)
# Interpret canonical variables: 
a1
# As variables 1,5,6 increase U1 increases.
# Variable 4 has the largest effect on U1.
b1
# V1 represents the different between variables 7 and 8.

# (e) 
Az = t(matrix(a1))
Bz = t(matrix(b1))
rho_U1_X1 = Az%*%S11
round(rho_U1_X1,4)
rho_V1_X2 = Bz%*%S22
round(rho_V1_X2,4)

# All loadings are negative, this is arbitrary and simply reflects the 
# direction in which the canonical variates are defined. 
# Variables 1 and 4 have strongest loadings. 
# U1 represents general accounting profitability factor.
# V1 represents overall market-value profitability. 

#-------------------------------------------------------------------------------
# Question 2
# (a) 
# Canonical Correlation Analysis aims at determining the pair of linear
# combinations with the largest correlation. Although Var[X1(1)]>Var[X2(1)] 
# and Var[X2(2)]>Var[X1(2)], X1(1) has 0 correlation with X2(2) but X2(1) and 
# X1(2) have a correlation of 0.95. 
# SHOW THIS WITH THE FORMULA 

# (b) 
# There will be 0 correlation between the first canonical variables. 
# AGAIN SHOW THIS WITH THE FORMULA 

# (c) 
# Doubling the variance will decrease the correlation by a factor of sqrt(2). 
# The denominator increases while correlation stays the same, thus the 
# canonical correlation decreases. 

# (d) 
# Decrease to ensure unit variance. 
# SHOW CALCULATION 
