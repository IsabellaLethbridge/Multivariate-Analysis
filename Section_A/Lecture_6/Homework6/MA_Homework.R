# 24 March 2026
# Lecture 6
# Isabella Lethbridge 

# Exercise 6.1
rm(list = ls())
X1 <- read.table('Lecture_6/T6-11.dat')
X1 <- X1[-31,]
X2 <- read.table('Lecture_6/T5-12.dat')

# a 
plot(X1)

# b
# Group mean vectors 
X1_bar <- apply(X1, 2, mean)
X2_bar <- apply(X2, 2, mean)

# Group covariance vectors 
S1 <- cov(X1)
S2 <- cov(X2)

# No. observations oer group
nl <- c(nrow(X1), nrow(X2))

# No. categories per group
p <- ncol(X1)

# No. groups 
g <- 2

# Total observations 
n <- sum(nl)

# Means across categories 
Xbar <- (nl[1]*X1_bar + nl[2]*X2_bar)/n

# SSP within 
W <- (nl[1] - 1)*S1 + (nl[2] - 1)*S2

# SSP Between
B <- nl[1]*(X1_bar - Xbar)%*%t(X1_bar - Xbar) + nl[2]*(X2_bar - Xbar)%*%t(X2_bar - Xbar)

# Wilk's lambda 
lam <- det(W)/det(B + W)

# Test statistic 
# Special case: p = 2, g = 2
statt <- (n - g - 1)/(g - 1)*(1 - lam)/lam

# Differences between treatment effects 
tau_12 <- X1_bar - X2_bar

# Simultaneous Bonferroni CI's
alpha <- 0.05

options(digits = 2)
CI_12 <- cbind(tau_12, tau_12) + 
  qt(1 - alpha/(p*g*(g-1)), n - g)*sqrt(diag(W)/(n-g)*(1/nl[1] + 1/nl[2]))%*%t(c(-1, 1))
CI_12

# For quiz 
X1 <- as.matrix(X1, ncol = 2)
X2 <- as.matrix(X2, ncol = 2)
X <- rbind(X1, X2)
group <- c(rep(1, nl[1]), rep(2, nl[2]))
group <- as.factor(group)

fit1 <- aov(X ~ group)
summary(fit1)

# ==============================================================================

# Exercise 6.2 
# a
rm(list = ls())
dat <- read.table('Lecture_6/T6-17.dat')
attach(dat)
X <- cbind(V3, V4, V5)
V1 <- as.factor(V1)
V2 <- as.factor(V2)

# Fit two-way MANOVA
fit1 <- manova(X ~ V1*V2)
(summary1 <- summary(fit1, test = 'Wilks'))
# Significant interaction
# Factor 1 isn't necessarily significant 
# Factor 2 is significant 

#Note we can extract the SSP's if we want
attributes(summary1)
sum_stats <- summary1$SS
residuals <- sum_stats$Residuals

par(mfrow = c(1,3))
qqnorm(residuals[,1]); qqline(residuals[,1])
qqnorm(residuals[,2]); qqline(residuals[,2])
qqnorm(residuals[,3]); qqline(residuals[,3])

# Extra code 
#To conduct separate univariate ANOVA's
summary(aov(X ~ V1))
summary(aov(X ~ V2))

aov_mod <- aov(V4 ~ V1*V2)
summary(aov(X ~ V1*V2))

par(mfrow=c(3,2))
interaction.plot(V1, V2, X[,1], 
                 ylab = bquote(bar(x)[1]), xlab = 'Factor 1', 
                 col = c('blue3', 'red3'), trace.label = 'Factor2', leg.bty = 'o')
boxplot(X[,1] ~ V1*V2, ylab = expression(x[1]), xlab = 'Factor 1:Factor2')

interaction.plot(V1, V2, X[,2], 
                 ylab = bquote(bar(x)[2]), xlab = 'Factor 1', 
                 col = c('blue3', 'red3'), trace.label = 'Factor 2', leg.bty = 'o')
boxplot(X[,2] ~ V1*V2, ylab = expression(x[2]), xlab = 'Factor1:Factor2')

interaction.plot(V1, V2, X[,3], 
                 ylab = bquote(bar(x)[3]), xlab = 'Factor 1', 
                 col = c('blue3', 'red3'), trace.label = 'Factor 2', leg.bty = 'o')
boxplot(X[,3] ~ V1*V2, ylab = expression(x[3]), xlab = 'Factor1:Factor2')

library(DescTools)
PostHocTest(aov_mod, method = "bonf")
