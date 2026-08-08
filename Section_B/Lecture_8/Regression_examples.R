# Multivariate Analysis Section B: Least-squares regression examples
# 19 May 2026
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Multiple Regression Example
#-------------------------------------------------------------------------------
# Setup:
data(
  "data_tidy_house_price",
  package = "DataTidy23RodoHonsMult"
)
View(data_tidy_house_price)

# Load required packages:
library(tidyr)
library(dplyr)

# Extract response matrix:
y <- data_tidy_house_price |>
  select(selling_price)
y <- as.matrix(y)

# Extract predictor matrix and prepend column of 1's:
X <- data_tidy_house_price |>
  select(-selling_price)
X <- as.matrix(cbind(1, X), nrow = length(X), ncol = 2)
colnames(X)[1] <- c("intercept")

# Obtain estimated regression coefficients:
beta_hat <- solve(t(X)%*%X)%*%t(X)%*%y
colnames(beta_hat) <- c("estimate")

# Check that this matches lm():
lm(y~X)

# Calculate R-squared: 
y_bar <- mean(y)
SST <- sum((y-y_bar)^2)
SSE <- sum((y-X%*%beta_hat)^2)
SSR <- SST-SSE
R2 <- SSR/SST

#-------------------------------------------------------------------------------
# Multivariate Regression Example
#-------------------------------------------------------------------------------
# Setup: 
data(
  "data_tidy_paper",
  package = "DataTidy23RodoHonsMult"
)
View(data_tidy_paper)

# Extract response matrix:
Y <- data_tidy_paper |>
  select(starts_with("paper")) |>
  as.matrix()

# Extract predictor matrix:
X <- data_tidy_paper |>
  select(starts_with("pulp")) |>
  as.matrix()
X <- cbind(1, X)
colnames(X)[1] <- c("intercept")

# Obtain estimated regression coefficients:
Beta_hat <- solve(t(X)%*%X)%*%t(X)%*%Y

# Check that this matches lm():
lm(Y~X)

#-------------------------------------------------------------------------------
# Inference Example
#-------------------------------------------------------------------------------
# Use only paper_1:
Y <- as.data.frame(Y) |>
  select("paper_1") |>
  as.matrix()

# Obtain estimated coefficients:
beta_hat <- solve(t(X)%*%X)%*%t(X)%*%Y

# pulp_1 confidence interval:
XtX <- t(X)%*%X
inv_XtX <- solve(XtX)
n <- nrow(X)
k <- ncol(X)
var_resp <- sum((Y-X%*%beta_hat)^2)/(n-k)
sd_beta_pulp1 <- sqrt(var_resp)*sqrt(inv_XtX[2,2])
# t-distributed quantile:
t_beta_pulp1 <- qt(0.975,n-k)
# confidence interval:
lower <- beta_hat[2] - t_beta_pulp1*sd_beta_pulp1
upper <- beta_hat[2] + t_beta_pulp1*sd_beta_pulp1
lower;upper

# Hypothesis testing: B1 = 0:
t_stat <- beta_hat[2]/sd_beta_pulp1
# p-value:
pt(-abs(t_stat), n-k)*2
# Reject H0 at 11% 

# Hypothesis testing: B1 = B3 = B4 = 0:
# Extract C11 matrix:
C11_mat <- inv_XtX[2:5, 2:5]
C11_mat <- C11_mat[-2,-2]

# Extract relevant estimates:
B_1 <- beta_hat[c(2, 4:5), , drop = FALSE]
q <- nrow(B_1)
# F-statistic:
F_stat <- (t(B_1)%*%solve(C11_mat)%*%B_1)/(q*var_resp)
# p-value:
pf(F_stat, q, n-k, lower.tail = FALSE)

#-------------------------------------------------------------------------------
# Multivariate Inference Example:
#-------------------------------------------------------------------------------
# Extract response matrix:
Y <- data_tidy_paper |>
  select(starts_with("paper")) |>
  as.matrix()

# Extract predictor matrix:
X <- data_tidy_paper |>
  select(starts_with("pulp")) |>
  as.matrix()
X <- cbind(1, X)
colnames(X)[1] <- c("intercept")

# Extract data for predictor matrix excluding pulp_2 and pulp_3:
X_v2 <- X[,c(1,2,5), drop = FALSE]

# Obtain estimated regression coefficients under both models:
Beta_hat <- solve(t(X)%*%X)%*%t(X)%*%Y
Beta_hat_v2 <- solve(t(X_v2)%*%X_v2)%*%t(X_v2)%*%Y

# Calculate sample variance-covariance matrices under both models:
sigma <- (t(Y - X%*%Beta_hat)%*%(Y - X%*%Beta_hat))/(n)
sigma_v2 <- (t(Y - X_v2%*%Beta_hat_v2)%*%(Y - X_v2%*%Beta_hat_v2))/(n)

# Test statistic:
n <- nrow(Y)
r <- ncol(Y)
q <- ncol(X)-ncol(X_v2)
k <- ncol(X)
p <- k-1
test_stat <- -(n-k-0.5*(r-p+q+1))*(log(det(sigma)/det(sigma_v2)))
# p-value:
pchisq(test_stat, r*q, lower.tail = FALSE)
