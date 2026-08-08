# Multivariate Section B: Factor Analysis
# 10 June 2026
# Isabella Lethbridge

#-------------------------------------------------------------------------------
# Question 1
#-------------------------------------------------------------------------------
# Loading matrix:
L1 = matrix(c(0.9,0.7,0.5),nrow=3,ncol=1)

# Psi matrix:
Psi1 = matrix(c(0.19,0,0,0,0.51,0,0,0,0.75),nrow=3,ncol=3)

# Determine rho:
rho1 = L%*%t(L)+Psi
rho1

#-------------------------------------------------------------------------------
# Question 2
#-------------------------------------------------------------------------------
# Read in data: 
dat2 = read.csv("Multivariate_Analysis/Section_B/Lecture_9/CA9/Air Pollution Data .csv")

# (a)
# Generate sample correlation matrix:
R2 = cor(dat2)
R2

# (b)
# Eigen decomposition:
eig2 = eigen(R2)

# Determine loadings:
L2 = eig2$vectors%*%diag(sqrt(eig2$values))

# m=1:
l1 = L2[,1,drop=FALSE]
colnames(l1) = c("Factor 1")
rownames(l1) = colnames(dat2)
round(l1,3)

# m=2:
l2 = L2[,1:2,drop=FALSE]
colnames(l2) = c("Factor 1", "Factor 2")
rownames(l2) = colnames(dat2)
round(l2,3)

# Proportion of variance explained:
eig_val = eig2$values
total_var = sum(eig_val)

prop_var = eig_val/total_var
cum_var  = cumsum(prop_var)

# (c)
# Obtain maximul likelihood factor model:
ML_model = factanal(covmat=R2,
                    factors=2,
                    rotation='none')
ML_loadings = as.matrix(ML_model$loadings)
colnames(ML_loadings) = c("Factor 1", "Factor 2")
rownames(ML_loadings) = colnames(dat2)

print(ML_model$loadings,cutoff=0)

# (d)
# The factor loadings of the PC model are much larger than that of the ML model
# This is because PC method focuses on maximising the explained variance,
# often resulting in larger absolute loadings, compared to the ML method, which
# aims to produce more interpretable factors by reproducing the original 
# covariance structure. 
# The PC method produces relatively large absolute loadings for CO,NO,NO2,O3 
# and HC for factor 1, whereas the ML method produces large absolute loadings 
# for CO,NO and NO2 only on factor 1. 

# (e) 
l2_rot = varimax(l2)
print(l2_rot,cutoff=0)

# The varimax rotation produces more interpretable factors with CO, NO, NO2 and 
# HC loading strongly onto factor 1. Radiation and O3 load strongly on factor 2 with wind having a moderate loading.
# The rotation is not changed the proportion of variation explained by each 
# factor but simply shifted the proportion of variation each variable explains. 
# Factor 1 now represents atmospheric pollutants with gases (excluding O3) 
# loading strongly onto this factor.
# Factor 2 now represents pollution correlated with heat (??)

# (f) 
# Calculate factor scores: 
L       = ML_loadings
Psi_inv = diag(1/ML_model$uniquenesses)
Z       = scale(dat2)
A       = t(L)%*%Psi_inv%*%L 
B       = solve(A)%*%t(L)%*%Psi_inv

scores = Z%*%t(B)
scores
