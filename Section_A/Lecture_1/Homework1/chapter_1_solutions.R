### Homework exercise 1.1

# 1.
# pdf('ex_1_1_a.pdf')
# Data given in question
X <- as.data.frame(matrix(c(9, 5, 1,
                            1, 3, 2), 3))
xbar <- apply(X, 2, mean)

# Plot scatterplot
plot(X$V1, X$V2, xlim = c(0, 10), ylim=c(0,4), xlab = expression(X[1]), ylab = expression(X[2]),
     pch = 16)
abline(h = 0, v = 0)
points(t(xbar), pch = 13)
text(X$V1, X$V2 + 0.15, c('(9, 1)', '(5, 3)', '(1, 2)'))
text(xbar[1], xbar[2] + 0.15, bquote(bar(x) * ' = (5, 2)'))
# dev.off()

# 2.
library(plot3D)
cent_X <- scale(X, center = T, scale = F)
# Obtain deviation vectors
d1 <- matrix(cent_X[,1])
d2 <- matrix(cent_X[,2])

# Plotting requires matrix
Xp <- as.matrix(X)

# pdf('ex_1_1_b.pdf')
# Fist plot the unit vector
lines3D(x = c(0, 10), y = c(0, 10), z = c(0, 10), 
         lwd = 3, bty = 'g', colkey = F,
         xlim=c(-5, 10), ylim=c(-2, 10), zlim=c(-5, 10), ticktype = 'detailed',
         theta = 0, phi = 30, #can change view if we want to
         xlab = 'n1', ylab = 'n2', zlab = 'n3')

# Plot the data vectors
arrows3D(x0 = rep(0, 2), y0 = rep(0, 2), z0 = rep(0, 2), x1 = Xp[1,], y1 = Xp[2,], z1 = Xp[3,], 
         lwd = 2, col = c(2, 2), add = T, colkey = F, type = 'cone')

# Add labels to the arrows
text3D(x = Xp[1,], y = Xp[2,], z = Xp[3,], c(expression(X[1]), expression(X[2])), add = T)

# Now add the deviation vectors
arrows3D(x0 = xbar, y0 = xbar, z0 = xbar, x1 = Xp[1,], y1 = Xp[2,], z1 = Xp[3,], 
         lwd = 2, col = c(3, 3), add = T, colkey = F, type = 'cone')

text3D(xbar, xbar, xbar, c(expression(d[1]), expression(d[2])), add = T)
# dev.off()

# Let's get an interactive plot using rgl:

library(plot3Drgl)
plotrgl(windowRect = c(8,30,900, 850))
play3d(spin3d(axis = c(0, 0, -1), rpm = 4), duration = 15)



# 3.
# Now sketch as vectors 
D <- as.matrix(cbind(d1, d2))
# pdf('ex_1_1_c.pdf')
# Get a different view to show the angle between the vectors
arrows3D(x0 = rep(0, 2), y0 = rep(0, 2), z0 = rep(0, 2), x1 = D[1, ], y1 = D[2, ],z1 = D[3, ], 
         lwd = 2, colvar = c(1,2), colkey = FALSE,
         xlim=c(-2, 5), ylim=c(-2, 5), zlim=c(-5, 2), ticktype = 'detailed',
         xlab = 'n1', ylab = 'n2', zlab = 'n3', theta = -30, phi = 0)
text3D(-0.5, -0.5, -0.5, expression(theta[12]), add = T)
text3D(x = D[1, ], y = D[2, ],z = D[3, ], c(expression(d[1]), expression(d[2])), add = T)
# dev.off()


### Homework exercise 1.2

xbar <- c(0.766, 0.508, 0.438, 0.161)
S <- matrix(c(0.856, 0.635, 0.173, 0.096, 0.635, 0.568, 0.128, 0.067, 0.173, 0.128, 0.171, 0.039, 0.096, 0.067, 0.039, 0.043), nrow = 4)

# 1.
b <- c(1,1,1,1)

# Mean of total consumption
t(b) %*% xbar

# Variance of total consumption
t(b) %*% S %*%b

#2.
c <- c(1, -1, 0, 0)

# Mean of excess
t(c) %*% xbar

# Variance of excess
t(c) %*% S %*%c

# Covariance between total and excess
t(b) %*% S %*%c
