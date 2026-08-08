# Multivariate Analysis Section B: Factor Analysis 
# 13 July 2026 
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Question 1
L   = matrix(c(0.9,0.7,0.5),nrow=3,ncol=1)
psi = matrix(c(0.19,0,0,0,0.51,0,0,0,0.75),nrow=3,ncol=3)

rho = L%*%t(L)+psi
rho

#-------------------------------------------------------------------------------
# Question 2
# Read in data: 
library(readr)
dat2 = read.csv("Section_B/Lecture_9/CA9/Air Pollution Data .csv")

# (a)
# Generate a sample correlation matrix:
cor_mat = cor(dat2)
round(cor_mat,2)

# (b)
# Obtain principal component solution: 
eig_mat = eigen(cor_mat)
eig_vec = eig_mat$vectors
eig_val = eig_mat$values
round(eig_vec,2)
round(eig_val,2)

# For m=1:
L1 = as.matrix(eig_vec[,1])%*%sqrt(eig_val[1])
round(L1,4)
prop_var1 = eig_val[1]/sum(eig_val)
round(prop_var1,2)

# For m=2:
L2 = eig_vec[,1:2]%*%diag(sqrt(eig_val[1:2]))
round(L2,4)
prop_var2 = eig_val[1:2]/sum(eig_val)
round(prop_var2,2)

# (c)
# Maximum likelihood factor model:
mle_mod = factanal(dat2,factors=2)
print(mle_mod,cutoff=0)

# (d) 
# The principal component solution results in factor 1 with high loadings for 
# all 7 of the variables, this is not easily interpretable. 
# The MLE approach, through the use of a varimax rotation results in factor 1
# with high loadings on CO,NO,NO2 (atmospheric pollutants) and factor 2 with 
# high loadings on O3 and moderate loadings on Radiation, CO and Wind.
# The use of a varimax rotation results in more interpretable factors. 

# (e) 
# Varimax rotation of principal component solution:
pc_mod = varimax(L2)
print(pc_mod$loadings,cutoff=0)

# Varimax rotation results in more interpretable factors. Factor 1 has strong 
# loadings for CO,NO,NO2, similar to what was obtained using MLE approach.
# Factor 2 has strong loadings for O3 and moderate loadings for Radiation, CO 
# and Wind. 

# (f)
# Calculate factor scores:
L = as.matrix(mle_mod$loadings)
Psi = diag(mle_mod$uniquenesses)
X_centered = t(scale(dat2,center=TRUE,scale=TRUE))

f_hat = t(solve(t(L)%*%solve(Psi)%*%L)%*%t(L)%*%solve(Psi)%*%X_centered)
head(f_hat)
