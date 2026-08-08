### Multivariate Honours

## Chapter 5

## J&W Example 5.2

sweat <- read.table('T5-1.dat')
head(sweat)
n <- nrow(sweat); p <- ncol(sweat)

# Obtain mean vector
(xbar <- matrix(apply(sweat, 2, mean), 3))

# Calculate covariance matrix
(S <- cov(sweat))

# H0: mu = mu0. Define mu0
(mu0 <- matrix(c(4, 50, 10), 3))

# Calculate Hotteling's T^2
(T2 <- n*t(xbar - mu0)%*%solve(S)%*%(xbar - mu0))

# Calculate p-value
1 - pf(T2*(n-p)/((n-1)*p), p, n-p)

# This sample is quite unlikely to occur if H_0 is true.
# Therefore we have reasonably strong evidence against H_0
# and we will reject H_0 at any level of significance above 6.5%.

## Checking Normality

colnames(sweat) <- c('Sweat_rate', 'Sodium', 'Potassium')
#Marginal qq-plots
par(mfrow=c(2,2))
qqnorm(sweat$Sweat_rate, main = 'Sweat Rate', pch = 16)
qqline(sweat$Sweat_rate)
qqnorm(sweat$Sodium, main = 'Sodium', pch = 16)
qqline(sweat$Sodium)
qqnorm(sweat$Potassium, main = 'Potassium', pch = 16)
qqline(sweat$Potassium)

#Chi-square plot
d2 <- mahalanobis(sweat, colMeans(sweat), cov = S)
d_rank <- rank(d2)
q <- qchisq((d_rank-0.5)/n, p)
plot(d2 ~ q, main = 'Chi-square plot', pch = 16)
abline(a=0, b=1, col = 'red')

# Comparing to 50% quantile
mean(d2 < qchisq(0.5, p))

## Bivariate plots
library(ggplot2)
b1 <- ggplot(sweat[, 1:2], aes(x = Sweat_rate, y = Sodium)) + geom_point() 
b1 + geom_density2d() + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
b2 <- ggplot(sweat[, c(1,3)], aes(x = Sweat_rate, y = Potassium)) + geom_point() 
b2 + geom_density2d() + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
b3 <- ggplot(sweat[, 2:3], aes(x = Sodium, y = Potassium)) + geom_point() 
b3 + geom_density2d() + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)



## Confidence Regions

## J&W Example 5.3

door_closed <- read.table('T4-1.DAT')
door_open <- read.table('T4-5.DAT')
mwave <- cbind(door_closed^0.25, door_open^0.25)
colnames(mwave) <- c('x1', 'x2')
head(mwave)

n <- nrow(mwave)
p <- ncol(mwave)
S <- cov(mwave)
xbar <- apply(mwave, 2 ,mean)


#H_0 as given in example
mu0 <- matrix(c(0.562, 0.589),2)

distance <- n*t(xbar - mu0)%*%solve(S)%*%(xbar - mu0)

#Using alpha = 5%
alpha <- 0.05
c2 <- ((n-1)*p)/(n-p)*qf(1-alpha, p, n-p)

#Is distance < c2?
distance
c2
#Yes, region contains mu0
#Therefore we don't reject H0

eigen <- eigen(S)

#Axis lengths
(axis1length <- sqrt(eigen$values[1])*sqrt(c2/n))
(axis2length <- sqrt(eigen$values[2])*sqrt(c2/n))

(axisratio <- sqrt(eigen$values[1])/sqrt(eigen$values[2]))
#Major axis is 3.1 times longer than minor axis

#Draw the ellipse, same as in textbook
par(mfrow=c(1,1))
phi <- atan2(eigen$vectors[2, 1], eigen$vectors[1, 1]) # angle of major axis
t <- seq(0, 2*pi, 0.01) 
xx <- xbar[1] + axis1length*cos(t)*cos(phi) - axis2length*sin(t)*sin(phi)
yy <- xbar[2] + axis1length*cos(t)*sin(phi) + axis2length*sin(t)*cos(phi)
plot(xx, yy, type = 'l', lwd = 2.5, xlab = '', ylab = '')
title(xlab = bquote(bar(x)[1]), line = 0.2)
title(ylab = bquote(bar(x)[2]), line = 0)
lines(c(xbar[1], xbar[1]), c(0, xbar[2]), lty = 5)
lines(c(0, xbar[1]), c(xbar[2], xbar[2]), lty = 5)

mjrx <- xbar[1] + c(-1, 1)*axis1length*cos(phi)
mjry <- xbar[2] + c(-1, 1)*axis1length*sin(phi)
mnrx <- xbar[1] + c(-1, 1)*axis2length*cos(phi + pi/2) #axes are always orthogonal!
mnry <- xbar[2] + c(-1, 1)*axis2length*sin(phi + pi/2)

lines(mjrx, mjry, lwd = 2)
lines(mnrx, mnry, lwd = 2)

points(mu0[1], mu0[2], pch = 16)
text(mu0[1], mu0[2], paste0('(', mu0[1], ', ', mu0[2], ')'), pos = 1)


## Simultaneous T^2 Intervals
mu1_lower <- xbar[1] - sqrt(c2)*sqrt(S[1,1]/n)
mu1_upper <- xbar[1] + sqrt(c2)*sqrt(S[1,1]/n)
mu2_lower <- xbar[2] - sqrt(c2)*sqrt(S[2,2]/n)
mu2_upper <- xbar[2] + sqrt(c2)*sqrt(S[2,2]/n)

#Plot
plot(xx, yy, type = 'l', lwd = 2.5, xlab = bquote(mu[1]), ylab = bquote(mu[2]))
lines(c(0, mu1_upper), c(mu2_upper, mu2_upper), lty = 5)
lines(c(0, mu1_upper), c(mu2_lower, mu2_lower), lty = 5)
lines(c(mu1_lower, mu1_lower), c(0, mu2_upper), lty = 5)
lines(c(mu1_upper, mu1_upper), c(0, mu2_upper), lty = 5)


## Bonferroni intervals
(round(xbar[1] - qt(1-alpha/(2*p), n-1)*sqrt(S[1,1]/n), 3))
(round(xbar[1] + qt(1-alpha/(2*p), n-1)*sqrt(S[1,1]/n), 3))
(round(xbar[2] - qt(1-alpha/(2*p), n-1)*sqrt(S[2,2]/n), 3))
(round(xbar[2] + qt(1-alpha/(2*p), n-1)*sqrt(S[2,2]/n), 3))



## Repeated measures
dental <- read.table('T6_16_DENTAL.DAT')
head(dental, 12)

boys <- dental[dental$V1 == 2, -1] #Extract boys' data
xbar <- apply(boys, 2, mean)
S <- var(boys)
n <- nrow(boys)
q <- ncol(boys)

#Contrast matrix 1
(C1 <- cbind(1, diag(-1, q-1)))

#Test statistic and p-value
(T2 <- n*t(C1%*%xbar) %*% solve(C1%*%S%*%t(C1)) %*% (C1%*%xbar))
1 - pf(T2*(n-q+1)/((n-1)*(q-1)), q-1, n-q+1)
