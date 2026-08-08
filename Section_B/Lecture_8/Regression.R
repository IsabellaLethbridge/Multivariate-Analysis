# Multivariate Section B: Regression
# 24 May 2026
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Setup
# Question 1:
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
library(tidyr)
library(dplyr)
data("data_tidy_yield", package = "DataTidyRodoSTA2005S")
View(data_tidy_yield)

#-------------------------------------------------------------------------------
# Question 1
# (a)
# Estimate regression coefficents:
# maze yield against soil PH and distance to water:
Y1 <- data_tidy_yield$CropYield |>
  as.matrix()

X1 <- data_tidy_yield |>
  select(SoilPH, DistanceToWater) |>
  as.matrix()
X1 <- cbind(1, X1)
colnames(X1)[1] <- c("intercept")

beta1 <- solve(t(X1)%*%X1)%*%t(X1)%*%Y1
check1 <- lm(Y1~X1)
                      
# sunlight against soil PH and distance to water:
Y2 <- data_tidy_yield$Sunlight |>
  as.matrix()

X2 <- data_tidy_yield |>
  select(SoilPH, DistanceToWater) |>
  as.matrix()
X2 <- cbind(1, X2)
colnames(X2)[1] <- c("intercept")

beta2 <- solve(t(X2)%*%X2)%*%t(X2)%*%Y2
check2 <- lm(Y2~X2)

# maize yield and sunlight against soil PH and distance to water: 
Y3 <- data_tidy_yield |>
  select(CropYield, Sunlight) |>
  as.matrix()

X3 <- data_tidy_yield |>
  select(SoilPH, DistanceToWater) |>
  as.matrix()
X3 <- cbind(1, X3)
colnames(X3)[1] <- c("intercept")

beta3 <- solve(t(X3)%*%X3)%*%t(X3)%*%Y3
check3 <- lm(Y3~X3)

# (b) 
# Confidence interval for effect of soil PH on temperature: 
Y4 <- data_tidy_yield |>
  select(TempC) |>
  as.matrix()

X4 <- data_tidy_yield |>
  select(SoilPH) |>
  as.matrix()
X4 <- cbind(1, X4)
colnames(X4)[1] <- c("intercept")

beta4 <- solve(t(X4)%*%X4)%*%t(X4)%*%Y4
check4 <- lm(Y4~X4)

XtX4 <- t(X4)%*%X4
inv_XtX4 <- solve(XtX4)
n <- nrow(Y4)
k <- ncol(X4)
s2_4 <- (t(Y4 - X4%*%beta4)%*%(Y4-X4%*%beta4))/(n-k)
se_4 <- sqrt(s2_4*inv_XtX4[2,2])

t_val <- abs(qt(0.975,n-k,lower.tail = FALSE))

# Confidence intervals:
lower4 <- beta4[2,1] - t_val*se_4
upper4 <- beta4[2,1] + t_val*se_4
lower4;upper4

# (c)
# F-test for the effect of soil PH and distance to water on maize yield:
XtX1 <- t(X1)%*%X1
C <- solve(XtX1)
C11 <- C[2:3,2:3]
inv_C11_1 <- solve(C11)

n <- nrow(Y1)
k <- ncol(X1)
q <- 2
s2_1 <- ((t(Y1 - X1%*%beta1))%*%(Y1-X1%*%beta1))/(n-k)

beta1_hat <- beta1[2:3,drop=FALSE]
f_stat1 <- as.numeric((t(beta1_hat)%*%inv_C11_1%*%beta1_hat)/(q*s2_1))
p_val1 <- 1-pf(f_stat1,q,n-k)

# (d)
# Likelihood ratio test:
Y5 <- data_tidy_yield |>
  select(CropYield,Sunlight) |>
  as.matrix()

# Saturated model:
X5_full <- data_tidy_yield |>
  select(Rainfall,
         TempC,
         SoilPH,
         WindSpeed,
         DistanceToWater,
         Altitude) |>
  as.matrix()
X5_full <- cbind(1, X5_full)
colnames(X5_full)[1] <- c("intercept")

beta5_full <- solve(t(X5_full)%*%X5_full)%*%t(X5_full)%*%Y5
check5_full <- lm(Y5~X5_full)

# Remove soil PH:
X5 <- data_tidy_yield |>
  select(Rainfall,
         TempC,
         WindSpeed,
         DistanceToWater,
         Altitude) |>
  as.matrix()
X5 <- cbind(1, X5)
colnames(X5)[1] <- c("intercept")

beta5 <- solve(t(X5)%*%X5)%*%t(X5)%*%Y5
check5 <- lm(Y5~X5)

n <- nrow(Y5)
k <- ncol(X5_full)
q <- 1
r <- ncol(Y5)
p <- k-1

# Calculate RSS for both models:
sig_full <- (t(Y5-X5_full%*%beta5_full)%*%(Y5-X5_full%*%beta5_full))/(n)
sig <- (t(Y5-X5%*%beta5)%*%(Y5-X5%*%beta5))/(n)

lambda <- (det(sig_full)/det(sig))^(n/2)
stat <- -(n-k-0.5*(r-p+q+1))*log(lambda^(2/n))

chi_stat <- pchisq(stat,r*q,lower.tail=FALSE)

#-------------------------------------------------------------------------------
# Setup
# Question 2:
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("MiguelRodo/DataTidy23RodoHonsMult@2024")
data("data_tidy_mali", package = "DataTidy23RodoHonsMult")

#-------------------------------------------------------------------------------
# Question 2:
# Multiple regression:
Y <- data_tidy_mali |>
  select(cotton) |>
  as.matrix()
Y <- scale(Y,center=TRUE,scale=FALSE)

X <- data_tidy_mali |>
  select(-cotton) |>
  as.matrix()
X <- scale(X)
X <- cbind(1,X)
colnames(X)[1] <- c("intercept")

beta <- solve(t(X)%*%X)%*%t(X)%*%Y
check <- lm(Y~X[,-1])
y_hat <- X%*%beta
mse <- mean((Y-y_hat)^2)

# PCR:
