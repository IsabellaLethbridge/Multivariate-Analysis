# Multivariate Section B: Regression 
# 9 June 2026
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Question 1
#-------------------------------------------------------------------------------
# Setup
if (!requireNamespace("DataTidy23RodoSTA2005SAssignment", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    utils::install.packages("remotes")
  }
  remotes::install_github("MiguelRodo/DataTidy23RodoSTA2005SAssignment")
}
if (!requireNamespace("tibble", quietly = TRUE)) {
  install.packages("tibble")
}
library(tibble) # ensures dataframes print more informatively and concisely than standard methods
data("data_tidy_yield", package = "DataTidyRodoSTA2005S")
dat1 = data_tidy_yield

#-------------------------------------------------------------------------------
# (a)
n = nrow(dat1)
# crop yield against soil PH and distance to water:
Y1 = matrix(dat1$CropYield,nrow=n,ncol=1)
X1 = cbind(1,
           dat1$SoilPH,
           dat1$DistanceToWater)

# Estimate coefficients: 
beta_hat1 = solve(t(X1)%*%X1)%*%t(X1)%*%Y1
beta_hat1

# sunlight against soil PH and distance to water:
Y2 = matrix(dat1$Sunlight,nrow=n,ncol=1)
X2 = cbind(1,
           dat1$SoilPH,
           dat1$DistanceToWater)

# Estimate coefficients: 
beta_hat2 = solve(t(X2)%*%X2)%*%t(X2)%*%Y2
beta_hat2

# crop yield and sunlight against soil PH and distance to water: 
Y3 = cbind(dat1$CropYield,
           dat1$Sunlight)
X3 = cbind(1,
           dat1$SoilPH,
           dat1$DistanceToWater)

# Estimate coefficients:
beta_hat3 = solve(t(X3)%*%X3)%*%t(X3)%*%Y3
beta_hat3

#-------------------------------------------------------------------------------
# (b) 
# Confidence interval for effect of soil PH on temperature:
# Construct X and Y matrices: 
Y4 = matrix(dat1$TempC,nrow=n,ncol=1)
X4 = cbind(1,
           dat1$SoilPH)

# Estimate coefficients:
beta_hat4 = solve(t(X4)%*%X4)%*%t(X4)%*%Y4
beta_hat4

# Calculate s^2:
n = nrow(Y4)
k = ncol(X4)

Yhat4 = X4%*%beta_hat4
res4  = (Y4-Yhat4)
s4    = (t(res4)%*%res4)/(n-k)

# Confidence interval parameters: 
Bj       = beta_hat4[2]
tstat    = qt(0.975,df=n-k)
XtX4     = t(X4)%*%X4
inv_XtX4 = solve(XtX4)

lower4 = Bj - tstat*sqrt(s4%*%inv_XtX4[2,2])
upper4 = Bj + tstat*sqrt(s4%*%inv_XtX4[2,2])
lower4;upper4

#-------------------------------------------------------------------------------
# (c) 
# F-test for soil PH and distance to water on crop yield:
# This is looking at X1 and Y1 
# F-test parameters: 
C1  = solve(t(X1)%*%X1)
C22 = C1[2:3,2:3]
B22 = beta_hat1[2:3,,drop=FALSE]
q   = nrow(B22)        # no. predictors 

# Determine s^2
n = nrow(Y1)
k = ncol(X1)

Yhat1 = X1%*%beta_hat1
res1  = (Y1-Yhat1)
s1    = (t(res1)%*%res1)/(n-k)

# Calculate F-stat:
Fstat1 = (t(B22)%*%solve(C22)%*%B22)/(q*s1)
Fstat1

# Calculate p-value:
# These are the exact same
pval1 = 1-pf(Fstat1,df1=q,df2=n-k)
pval1 = pf(Fstat1,df1=q,df2=n-k,lower.tail=FALSE)
pval1

#-------------------------------------------------------------------------------
# (d) 
# Likelihood ratio test for soil PH on crop yield and sunlight
Y = cbind(dat1$CropYield,
          dat1$Sunlight)
n = nrow(Y)

# Full model: 
Xfull = cbind(1,
              dat1$Rainfall,
              dat1$TempC,
              dat1$SoilPH,
              dat1$WindSpeed,
              dat1$DistanceToWater,
              dat1$Altitude)

# Estimate coefficients: 
beta_hatfull = solve(t(Xfull)%*%Xfull)%*%t(Xfull)%*%Y

# MLE: 
Yhatfull = Xfull%*%beta_hatfull
resfull  = Y-Yhatfull
MLEfull  = (t(resfull)%*%resfull)/n

# Restricted model:
Xres = cbind(1,
             dat1$Rainfall,
             dat1$TempC,
             dat1$WindSpeed,
             dat1$DistanceToWater,
             dat1$Altitude)

# Estimate coefficients:
beta_hatres = solve(t(Xres)%*%Xres)%*%t(Xres)%*%Y

# MLE: 
Yhatres = Xres%*%beta_hatres
resres  = Y-Yhatres
MLEres  = (t(resres)%*%resres)/n

# Calculate lambda:
lambda = (det(MLEfull)/det(MLEres))^(n/2)

# Determine parameters:
k = ncol(Xfull)            # no. predictors (in full model)
r = ncol(Y)                # no. responses 
p = k-1                    # no. predictors excluding intercept 
q = ncol(Xfull)-ncol(Xres) # diff in no. predictors between full and restricted 

# Calculate test statistic:
test_stat = -(n-k-0.5*(r-p+q+1))*log(lambda^(2/n))
test_stat

# Calculate p-value:
pval = pchisq(test_stat,df=r*q,lower.tail=FALSE)
pval

#-------------------------------------------------------------------------------
# Question 2
#-------------------------------------------------------------------------------
# Setup
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("MiguelRodo/DataTidy23RodoHonsMult@2024")
data("data_tidy_mali", package = "DataTidy23RodoHonsMult")
dat2 = data_tidy_mali

#-------------------------------------------------------------------------------
# Multiple regression:
n = nrow(dat2)

# Construct X and Y matrices:
Y0 = matrix(dat2$cotton,nrow=n,ncol=1)
Y0 = scale(Y0)

X0 = cbind(dat2$family,
           dat2$dist_rd,
           dat2$maize,
           dat2$sorg,
           dat2$millet,
           dat2$bull,
           dat2$cattle,
           dat2$goats)
X0 = scale(X0)
X0 = cbind(1,X0)

# Estimate coefficients:
beta_hat0 = solve(t(X0)%*%X0)%*%t(X0)%*%Y0
beta_hat0

# Calculate MSE:
k = ncol(X0)

Yhat0 = X0%*%beta_hat0
res0 = (Y0-Yhat0)
MSE0 = mean((res0)^2)
MSE0

#-------------------------------------------------------------------------------
# PCR
# Obtain variance-covariance matrix:
cov_mat = cov(X0[,-1])

# Eigen decomposition:
eig = eigen(cov_mat)

# Extract eigen vectors and eigen values:
eig_vec = eig$vectors # loadings 
eig_val = eig$values  # variance explained 

# Elbow plot:
total_var = sum(eig_val)
prop_var  = eig_val/total_var
pc_seq    = 1:length(eig_val)
plot(pc_seq,prop_var,
     type='b',
     col="deeppink")
# We chose 6 as the optimal no. PCs (should lowkey be 2) 

# Obtain loadings:
loadings = eig_vec[,1:6]

# Obtain scores:
scores = X0[,-1]%*%loadings

# Estimate coefficients:
Xpcr = cbind(1,scores)
beta_hatpcr = solve(t(Xpcr)%*%Xpcr)%*%t(Xpcr)%*%Y0
beta_hatpcr

# Calculate MSE:
k = ncol(Xpcr)

Yhatpcr = Xpcr%*%beta_hatpcr
respcr = (Y0-Yhatpcr)
MSEpcr = mean((respcr)^2)
MSEpcr

#-------------------------------------------------------------------------------
# PLS
pls_mod = plsr(cotton~family+dist_rd+maize+sorg+millet+bull+cattle+goats,
               data=dat2,
               scale=TRUE,
               validation='CV')
summary(pls_mod)

plot(RMSEP(pls_mod))
