### Multivariate Honours

## Chapter 6

## Example 6.7

#Form data vectors
x1 <- matrix(c(9, 6, 9), 3)
x2 <- matrix(c(0, 2), 2)
x3 <- matrix(c(3, 1, 2), 3)

#Define n's
nl <- c(length(x1), length(x2), length(x3))

## First calculate everything from scratch

#Calculate Group means
xl_bar <- c(mean(x1), mean(x2), mean(x3))

#Calculate overall mean
xbar <- sum(x1, x2, x3)/sum(nl)

#Deviations between group means and overall mean
x_tr <- xl_bar - xbar

#Deviations between observations and group means
x1_res <- x1 - xl_bar[1]
x2_res <- x2 - xl_bar[2]
x3_res <- x3 - xl_bar[3]

#Deviations between observations and overall mean
x1_tot <- x1 - xbar
x2_tot <- x2 - xbar
x3_tot <- x3 - xbar

#Calculate Sums of Squares
(SS_tr <- t(nl) %*% x_tr^2)
(SS_res <- t(x1_res)%*%x1_res + t(x2_res)%*%x2_res + t(x3_res)%*%x3_res)
(SS_tot <- t(x1_tot)%*%x1_tot + t(x2_tot)%*%x2_tot + t(x3_tot)%*%x3_tot)

#Check that it matches sum
SS_tr + SS_res

#Calculate F-statistic
(F <- (SS_tr/(length(nl)-1))/(SS_res/(sum(nl) - length(nl))))

#Calculate p-value
(pval <- 1 - pf(F, length(nl)-1, sum(nl) - length(nl)))


#And check using aov function
X <- c(x1, x2, x3)
group <- c(rep(1, nl[1]), rep(2, nl[2]), rep(3, nl[3]))
group <- as.factor(group)
fit <- aov(X ~ group) 
summary(fit)

#Small p-value: 
#At least one mean is significantly different from the others

#Another way of getting the same output
anova(lm(X ~ group))



## Example 6.9

#Form data matrices
X1 <- matrix(c(9,6,9,3,2,7), 3)
X2 <- matrix(c(0,2,4,0), 2)
X3 <- matrix(c(3,1,2,8,9,7), 3)

#Define n's and p
nl <- c(nrow(X1), nrow(X2), nrow(X3))
p <- ncol(X1)

## First calculate everything from scratch

#Calculate Group means
X1_bar <- apply(X1, 2, mean)
X2_bar <- apply(X2, 2, mean)
X3_bar <- apply(X3, 2, mean)

#Calculate overall mean
Xbar <- (nl[1]*X1_bar + nl[2]*X2_bar + nl[3]*X3_bar)/sum(nl)

#Deviations between group means and overall mean
X1_tr <- X1_bar - Xbar
X2_tr <- X2_bar - Xbar
X3_tr <- X3_bar - Xbar

#Deviations between observations and group means
X1_res <- X1 - matrix(rep(X1_bar, each = nl[1]), nl[1])
X2_res <- X2 - matrix(rep(X2_bar, each = nl[2]), nl[2])
X3_res <- X3 - matrix(rep(X3_bar, each = nl[3]), nl[3])

#Deviations between observations and overall mean
X1_tot <- X1 - matrix(rep(Xbar, each = nl[1]), nl[1])
X2_tot <- X2 - matrix(rep(Xbar, each = nl[2]), nl[2])
X3_tot <- X3 - matrix(rep(Xbar, each = nl[3]), nl[3])

#Calculate Sums of Squares
(B <- nl[1]*X1_tr%*%t(X1_tr) + nl[2]*X2_tr%*%t(X2_tr) + nl[3]*X3_tr%*%t(X3_tr))
(W <- t(X1_res)%*%X1_res + t(X2_res)%*%X2_res + t(X3_res)%*%X3_res)
(SSP_tot <- t(X1_tot)%*%X1_tot + t(X2_tot)%*%X2_tot + t(X3_tot)%*%X3_tot)

#Check that it matches sum
B + W

#Calculate Wilks' lambda
(Lam <- det(W)/det(W + B))

#p = 2 and g = 3 (special case)
#Calculate test statistic
(F <- (sum(nl) - p - 2)/p*(1-sqrt(Lam))/sqrt(Lam))

#Calculate p-value
(pval <- 1 - pf(F, 2*p, 2*(sum(nl) - p - 2)))


#And check using manova function
X <- rbind(X1, X2, X3)
group <- c(rep(1, nl[1]), rep(2, nl[2]), rep(3, nl[3]))
group <- as.factor(group)
fit <- manova(X ~ group) 
summary(fit, 'Wilks')

#Small p-value: 
#At least one mean is significantly different from the others

#We can also extract the univariate anova's
summary.aov(fit)

#We specified 'Wilks', what are the other options?
summary(fit, '?')



## Example 6.11

#Read in data
load('J_WEx6.11.RData')
nl <- c(271, 138, 107) ; p <- 4 ; g <- 3
n <- sum(nl)

#SSP Within
W <- (nl[1] - 1)*S1 + (nl[2] - 1)*S2 + (nl[3] - 1)*S3

#SSP Between
Xbar <- (nl[1]*X1_bar + nl[2]*X2_bar + nl[3]*X3_bar)/n
B <- nl[1]*(X1_bar - Xbar)%*%t(X1_bar - Xbar) + nl[2]*(X2_bar - Xbar)%*%t(X2_bar - Xbar) + 
  nl[3]*(X3_bar - Xbar)%*%t(X3_bar - Xbar)

#Wilks' Lambda
Lam <- det(W)/(det(B+W))

#p = 2 and g = 3 (special case)
#Calculate test statistic
(F <- (n - p - 2)/p*(1-sqrt(Lam))/sqrt(Lam))

#Calculate p-value
(pval <- 1 - pf(F, 2*p, 2*(n - p - 2)))

#Compare with large sample approximation
(c <- -(n - 1 - (p+g)/2)*log(Lam))
1 - pchisq(c, p*(g-1))

#There is definitely a difference in the mean vectors...
#...which ones?

#Differences between treatment effects
tau_12 <- X1_bar - X2_bar
tau_13 <- X1_bar - X3_bar
tau_23 <- X2_bar - X3_bar

#set alpha
alpha <- 0.05

#Calculate simultaneous Bonferroni CI's
options(digits = 2)
CI_12 <- cbind(tau_12, tau_12) + 
  qt(1 - alpha/(p*g*(g-1)), n - g)*sqrt(diag(W)/(n-g)*(1/nl[1] + 1/nl[2]))%*%t(c(-1, 1))
CI_12
CI_13 <- cbind(tau_13, tau_13) + 
  qt(1 - alpha/(p*g*(g-1)), n - g)*sqrt(diag(W)/(n-g)*(1/nl[1] + 1/nl[3]))%*%t(c(-1, 1))
CI_13
CI_23 <- cbind(tau_23, tau_23) + 
  qt(1 - alpha/(p*g*(g-1)), n - g)*sqrt(diag(W)/(n-g)*(1/nl[2] + 1/nl[3]))%*%t(c(-1, 1))
CI_23

#Which intervals don't contain 0?
CI_contains_zero <- matrix(NA, p, 3)
colnames(CI_contains_zero) <- c('Private-Nonprofit', 'Private-Government', 'Nonprofit-Government')
CI_contains_zero[,1] <- (CI_12[,1] * CI_12[,2]) < 0
CI_contains_zero[,2] <- (CI_13[,1] * CI_13[,2]) < 0
CI_contains_zero[,3] <- (CI_23[,1] * CI_23[,2]) < 0
CI_contains_zero


## Example 6.13
#Read in the data
plastic <- read.table('T6-4.dat')
head(plastic)
attach(plastic)
X <- cbind(V3, V4, V5)

#Fit two-way MANOVA
fit1 <- manova(X ~ V1*V2)
(summary1 <- summary(fit1, test = 'Wilks'))
#No significant interaction
#Both factor effects are significant (could be explored further)

#Note we can extract the SSP's if we want
attributes(summary1)
summary1$SS

#To conduct separate univariate ANOVA's
summary(aov(X ~ V1))
summary(aov(X ~ V2))

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
