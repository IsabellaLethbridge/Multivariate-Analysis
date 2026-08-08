# Multivariate Analysis Section B: Regression 
# 10 July 2026 
# Isabella Lethbridge 

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
library(tibble) 
library(knitr)
data("data_tidy_yield", package = "DataTidyRodoSTA2005S")

#-------------------------------------------------------------------------------
# Question 1
# (a) 
# Estimate regression coefficients: 
# maize yield vs soil PH and dist to water: 
X1 = cbind(1,data_tidy_yield$SoilPH,data_tidy_yield$DistanceToWater)
Y1 = as.matrix(data_tidy_yield$CropYield)
beta1_hat = solve(t(X1)%*%X1)%*%t(X1)%*%Y1
beta1_hat

# Check using lm() fxn:
mod1 = lm(Y1~X1[,2:3])
mod1$coefficients

# sunlight vs soil PH and dist to water:
X2 = cbind(1,data_tidy_yield$SoilPH,data_tidy_yield$DistanceToWater)
Y2 = as.matrix(data_tidy_yield$Sunlight)
beta2_hat = solve(t(X2)%*%X2)%*%t(X2)%*%Y2
beta2_hat

# Check using lm() fxn:
mod2 = lm(Y2~X2[,2:3])
mod2$coefficients

# maize yield and sunlight vs soil PH and dist to water:
X3 = cbind(1,data_tidy_yield$SoilPH,data_tidy_yield$DistanceToWater)
Y3 = cbind(data_tidy_yield$CropYield,data_tidy_yield$Sunlight)
beta3_hat = solve(t(X3)%*%X3)%*%t(X3)%*%Y3
beta3_hat

# Check using lm() fxn:
mod3 = lm(Y3~X3[,2:3])
mod3$coefficients

# (b) 
# Confidence interval for soil PH on temperature:
X4 = cbind(1,data_tidy_yield$SoilPH)
Y4 = as.matrix(data_tidy_yield$TempC)
beta4_hat = solve(t(X4)%*%X4)%*%t(X4)%*%Y4
beta4_hat

# Check using lm() fxn:
mod4 = lm(Y4~X4[,2])
mod4$coefficients

XtX4     = t(X4)%*%X4
inv_XtX4 = solve(XtX4)
y_hat4   = X4%*%beta4_hat
res4     = Y4-y_hat4

n4 = nrow(X4)
k4 = ncol(X4)

var_est4 = (t(res4)%*%res4)/(n4-k4)
var_est4 

t_val4 = qt(0.975,n4-k4)
t_val4

lower4 = beta4_hat[2]-t_val4*(sqrt(var_est4)%*%sqrt(inv_XtX4[2,2]))
upper4 = beta4_hat[2]+t_val4*(sqrt(var_est4)%*%sqrt(inv_XtX4[2,2]))
lower4;upper4

# (c) 
# Perform an F-test for effect of soil PH and dist to water on maize yield:
# X1 = cbind(1,data_tidy_yield$SoilPH,data_tidy_yield$DistanceToWater)
# Y1 = as.matrix(data_tidy_yield$CropYield)
# beta1_hat = solve(t(X1)%*%X1)%*%t(X1)%*%Y1
# beta1_hat

C1     = solve(t(X1)%*%X1)
C11    = C1[2:3,2:3]
y_hat1 = X1%*%beta1_hat
res1   = Y1-y_hat1

q1 = nrow(beta1_hat1)
n1 = nrow(X1)
k1 = ncol(X1)

var_est1 = (t(res1)%*%res1)/(n1-k1)
var_est1

beta1_hat1 = beta1_hat[2:3,,drop=FALSE]

num1   = t(beta1_hat1)%*%solve(C11)%*%beta1_hat1
denom1 = q1%*%var_est1

F1 = num1/denom1
F1

Fstat1 = qf(0.975,q1,n1-k1)
Fstat1

p_val1 = 1-pf(F1,q1,n1-k1)
p_val1

# (d) 
# Likelihood ratio test for effect of soil PH on maize yield or sunlight:
# X3 = cbind(1,data_tidy_yield$SoilPH,data_tidy_yield$DistanceToWater)
# Y3 = cbind(data_tidy_yield$CropYield,data_tidy_yield$Sunlight)
# beta3_hat = solve(t(X3)%*%X3)%*%t(X3)%*%Y3
# beta3_hat

# Fit a full model:
Y = cbind(data_tidy_yield$CropYield,data_tidy_yield$Sunlight)
n = nrow(Y)

Xfull = cbind(1,
              data_tidy_yield$Rainfall,
              data_tidy_yield$TempC,
              data_tidy_yield$SoilPH,
              data_tidy_yield$WindSpeed,
              data_tidy_yield$DistanceToWater,
              data_tidy_yield$Altitude)
betafull_hat = solve(t(Xfull)%*%Xfull)%*%t(Xfull)%*%Y
betafull_hat

y_hatfull = Xfull%*%betafull_hat
resfull   = Y-y_hatfull

sigma_full = t(resfull)%*%resfull/n

Xres = cbind(1,
             data_tidy_yield$Rainfall,
             data_tidy_yield$TempC,
             data_tidy_yield$WindSpeed,
             data_tidy_yield$DistanceToWater,
             data_tidy_yield$Altitude)
betares_hat = solve(t(Xres)%*%Xres)%*%t(Xres)%*%Y
betares_hat

y_hatres = Xres%*%betares_hat
resres   = Y-y_hatres

sigma_res = t(resres)%*%resres/n

k = ncol(Xfull)
q = 1
r = ncol(Y)
p = ncol(Xfull)-1

lambda = (det(sigma_full)/det(sigma_res))
lambda

stat = -(n-k-0.5*(r-p+q+1))*log(lambda)
stat

p_val = 1-pchisq(stat,r*q)
p_val

#-------------------------------------------------------------------------------
# Setup
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("MiguelRodo/DataTidy23RodoHonsMult@2024")
data("data_tidy_mali", package = "DataTidy23RodoHonsMult")

#-------------------------------------------------------------------------------
# Question 2
# Multiple regression: 
# Response variable: 
Y          = cbind(data_tidy_mali$cotton)
Y_centered = scale(Y,center=TRUE,scale=FALSE) # Center response 

# Predictor variables:
X = cbind(data_tidy_mali$family,
          data_tidy_mali$dist_rd,
          data_tidy_mali$maize,
          data_tidy_mali$sorg,
          data_tidy_mali$millet,
          data_tidy_mali$bull,
          data_tidy_mali$cattle,
          data_tidy_mali$goats)
X_scaled = scale(X)              # Standardise predictors 
X_var    = cbind(1,X_scaled)     # Design matrix 

# Coefficients:
beta_hat_mr = solve(t(X_var)%*%X_var)%*%t(X_var)%*%Y_centered
beta_hat_mr

# Calculate MSE: 
n = nrow(X_var)
yhat_mr = X_var%*%beta_hat_mr
res_mr  = Y_centered-yhat_mr

mse_mr = (t(res_mr)%*%res_mr)/n
mse_mr

# PCR:
# Calculate correlation matrix:
cor_mat_pcr = cor(X)

# Eigen decomposition:
eig_mat = eigen(cor_mat_pcr)
eig_val = eig_mat$values
eig_vec = eig_mat$vectors

# Determine optimal no. components:
tot_var  = sum(eig_val)
prop_var = eig_val/tot_var
cum_var  = cumsum(prop_var)
pcs      = seq(1,length(eig_val)) 
plot(pcs,prop_var,type='b')

scores = X_scaled%*%eig_vec
Z      = scores[,1:6]

beta_hat_pcr = solve(t(Z)%*%Z)%*%t(Z)%*%Y_centered 
beta_hat_pcr

# Calculate MSE:
yhat_pcr = Z%*%beta_hat_pcr
res_pcr  = Y_centered-yhat_pcr

mse_pcr = (t(res_pcr)%*%res_pcr)/n
mse_pcr
