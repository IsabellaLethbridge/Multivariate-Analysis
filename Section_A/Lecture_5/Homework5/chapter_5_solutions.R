### Homework exercise 5.1

options(digits = 4)

library(xtable)

rm(list = ls())
load('J&WEx5.9.Rdata')
vars <- c('Weight', 'Body Length', 'Neck', 'Girth', 'Head Length', 'Head Width')

n <- 61
p <- length(xbar)

# 1. Large sample 95% Simultaneous confidence intervals

alpha <- 0.05
c2 <- qchisq(1-alpha, p)
Sim_CI <- cbind(xbar, xbar) + matrix(sqrt(c2)*sqrt(diag(S)/n)) %*% t(matrix(c(-1,1)))
rownames(Sim_CI) <- vars
colnames(Sim_CI) <- c('Lower', 'Upper')
# print(xtable(Sim_CI, type = 'latex'), file = '5.1.1.tex')


# 2. Large sample 95% Simultaneous confidence ellipse for mean weight & mean girth

eigen <- eigen(S[c(1,4), c(1,4)])
xb <- xbar[c(1, 4)]

#Axis lengths
(axis1length <- sqrt(eigen$values[1])*sqrt(c2/n))
(axis2length <- sqrt(eigen$values[2])*sqrt(c2/n))

#Draw the ellipse
phi <- atan2(eigen$vectors[2, 1], eigen$vectors[1, 1]) # angle of major axis
t <- seq(0, 2*pi, 0.01) 
xx <- xb[1] + axis1length*cos(t)*cos(phi) - axis2length*sin(t)*sin(phi)
yy <- xb[2] + axis1length*cos(t)*sin(phi) + axis2length*sin(t)*cos(phi)
sclx <- max(xx)*0.05 #Scaling factor
scly <- max(yy)*0.05 #Scaling factor

par(mfrow=c(1,1))
# pdf('5.1.2.pdf')
plot(xx, yy, type = 'l', lwd = 2.5, xlab = 'Weight', ylab = 'Girth',
     xlim = c(min(xx) - sclx, max(xx) + sclx), ylim = c(min(yy) - scly, max(yy) + scly))
points(xb[1], xb[2], pch = 16)
# dev.off()


# 3. 95% Bonferroni confidence intervals

alpha <- 0.05
cb <- qt(1-alpha/(2*p), n-1)
Bon_CI <- cbind(xbar, xbar) + matrix(cb*sqrt(diag(S)/n)) %*% t(matrix(c(-1,1)))
rownames(Bon_CI) <- vars
colnames(Bon_CI) <- c('Lower', 'Upper')
# print(xtable(Bon_CI, type = 'latex'), file = '5.1.3.tex')


# 4. Bonferoni rectangles

par(mfrow=c(1,1))
# pdf('5.1.4.pdf')
plot(xx, yy, type = 'l', lwd = 2.5, xlab = 'Weight', ylab = 'Girth',
     xlim = c(min(xx) - sclx, max(xx) + sclx), ylim = c(min(yy) - scly, max(yy) + scly))

lines(c(0, Sim_CI[1, 2]), c(Sim_CI[4, 2], Sim_CI[4, 2]), lty = 5)
lines(c(0, Sim_CI[1, 2]), c(Sim_CI[4, 1], Sim_CI[4, 1]), lty = 5)
lines(c(Sim_CI[1, 1], Sim_CI[1, 1]), c(0, Sim_CI[4, 2]), lty = 5)
lines(c(Sim_CI[1, 2], Sim_CI[1, 2]), c(0, Sim_CI[4, 2]), lty = 5)

lines(c(0, Bon_CI[1, 2]), c(Bon_CI[4, 2], Bon_CI[4, 2]), lty = 3)
lines(c(0, Bon_CI[1, 2]), c(Bon_CI[4, 1], Bon_CI[4, 1]), lty = 3)
lines(c(Bon_CI[1, 1], Bon_CI[1, 1]), c(0, Bon_CI[4, 2]), lty = 3)
lines(c(Bon_CI[1, 2], Bon_CI[1, 2]), c(0, Bon_CI[4, 2]), lty = 3)

legend('top', c('Large Sample Simultaneous',  'Bonferroni'), lty = c(5, 3))
# dev.off()


#5. Bonferroni interval for x6 - x5

xb5 <- xbar[6] - xbar[5]
Bon_5 <- xb5 + c(-1, 1)*qt(1-alpha/(2*(p+1)), n-1)*sqrt((S[6,6] + S[5,5] - 2*S[5,6])/n)
                           


### Homework exercise 5.2

rm(list = ls())
bear <- read.table('T1-4.txt')
bear
W <- bear[,1:4]
L <- bear[,5:8]
n <- nrow(bear)

# 1. 95% Simultaneous confidence intervals

p <- 4
xbar <- colMeans(L)
S <- cov(L)

alpha <- 0.05
c_L <- ((n-1)*p)/(n-p)*qf(1-alpha, p, n-p)
Sim_CI_1 <- cbind(xbar, xbar) + matrix(sqrt(c_L)*sqrt(diag(S)/n)) %*% t(matrix(c(-1,1)))
colnames(Sim_CI_1) <- c('Lower', 'Upper')
# print(xtable(Sim_CI_1, type = 'latex'), file = '5.2.1.tex')


# 2. 95% Simultaneous confidence intervals for increases

C <- cbind(diag(-1, 3), 0) + cbind(0, diag(1, 3))
S_delta <- C%*%S%*%t(C)
xbar_delta <- C%*%matrix(xbar)

c_delta <- ((n-1)*(p-1))/(n-p+1)*qf(1-alpha, p-1, n-p+1)

Sim_CI_2 <- cbind(xbar_delta, xbar_delta) + 
  matrix(sqrt(c_delta*diag(S_delta)/n)) %*% t(matrix(c(-1,1)))
diff_names <- c('Lngth3 - Lngth2', 'Lngth4 - Lngth3', 'Lngth5 - Lngth4')
rownames(Sim_CI_2) <- diff_names
colnames(Sim_CI_2) <- c('Lower', 'Upper')
# print(xtable(Sim_CI_2, type = 'latex'), file = '5.2.2.tex')

# 3. Confidence ellipse for mean increases

eigen <- eigen(S_delta[c(1,3), c(1,3)])
xb <- xbar_delta[c(1, 3)]

#Axis lengths
axis1length <- sqrt(eigen$values[1])*sqrt(c_delta/n)
axis2length <- sqrt(eigen$values[2])*sqrt(c_delta/n)

#Draw the ellipse
phi <- atan2(eigen$vectors[2, 1], eigen$vectors[1, 1]) # angle of major axis
t <- seq(0, 2*pi, 0.01) 
xx <- xb[1] + axis1length*cos(t)*cos(phi) - axis2length*sin(t)*sin(phi)
yy <- xb[2] + axis1length*cos(t)*sin(phi) + axis2length*sin(t)*cos(phi)
sclx <- max(xx)*0.1 #Scaling factor
scly <- max(yy)*0.2 #Scaling factor

par(mfrow=c(1,1))
# pdf('5.2.3.pdf')
plot(xx, yy, type = 'l', lwd = 2.5, xlab = 'Length3 - Length2', ylab = 'Length5 - Length4',
     xlim = c(min(xx) - sclx, max(xx) + sclx), ylim = c(min(yy) - scly, max(yy) + scly))
# dev.off()


# 4. Bonferroni intervals

alpha <- 0.05
c_bon <- qt(1-alpha/(2*(2*p - 1)), n-1) #m = 2p - 1 in general when considering all variables and all their successive differences
xbar_bon <- c(xbar, xbar_delta)
S_bon <- c(diag(S), diag(S_delta))

Bon_CI <- cbind(xbar_bon, xbar_bon) + matrix(c_bon*sqrt(S_bon/n)) %*% t(matrix(c(-1,1)))
rownames(Bon_CI) <- c(colnames(L), diff_names)
colnames(Bon_CI) <- c('Lower', 'Upper')
# print(xtable(Bon_CI, type = 'latex'), file = '5.2.4.tex')


# 5. Bonferoni rectangles

par(mfrow=c(1,1))
# pdf('5.2.5.pdf')
plot(xx, yy, type = 'l', lwd = 2.5, xlab = 'Length3 - Length2', ylab = 'Length5 - Length4',
     xlim = c(min(xx) - sclx, max(xx) + sclx), ylim = c(min(yy) - scly, max(yy) + scly))

lines(c(-50, Sim_CI_2[1, 2]), c(Sim_CI_2[3, 2], Sim_CI_2[3, 2]), lty = 5)
lines(c(-50, Sim_CI_2[1, 2]), c(Sim_CI_2[3, 1], Sim_CI_2[3, 1]), lty = 5)
lines(c(Sim_CI_2[1, 1], Sim_CI_2[1, 1]), c(-50, Sim_CI_2[3, 2]), lty = 5)
lines(c(Sim_CI_2[1, 2], Sim_CI_2[1, 2]), c(-50, Sim_CI_2[3, 2]), lty = 5)

lines(c(-50, Bon_CI[5, 2]), c(Bon_CI[7, 2], Bon_CI[7, 2]), lty = 3)
lines(c(-50, Bon_CI[5, 2]), c(Bon_CI[7, 1], Bon_CI[7, 1]), lty = 3)
lines(c(Bon_CI[5, 1], Bon_CI[5, 1]), c(-50, Bon_CI[7, 2]), lty = 3)
lines(c(Bon_CI[5, 2], Bon_CI[5, 2]), c(-50, Bon_CI[7, 2]), lty = 3)

legend('top', c(expression(paste('Simultaneous ', T^2)),  'Bonferroni'), lty = c(5, 3))
# dev.off()