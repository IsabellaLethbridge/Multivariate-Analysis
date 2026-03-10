#10 March 2026 
#Chapter 1 Homework Exercises 
#Isabella Lethbridge 

###################################Exercise 1###################################

#Question 1
#coordinates in 2D
x <- c(9,5,1)
y <- c(1,3,2)

#Scatterplot
plot(x,y)

#Determine means 
xbar <- mean(x)
ybar <- mean(y)

#Scatterplot including balance point 
xval <- c(x, xbar)
yval <- c(y, ybar)

plot(xval, yval)


#Question 2
X <- matrix(c(9,5,1,1,3,2), nrow = 3)
xbar <- apply(X, 2, mean)

#Center X
Xc <- scale(X, center = T, scale = F)
Xc 

#Deviation vectors 
d1 <- matrix(Xc[,1])
d2 <- matrix(Xc[,2])

#Draw 3D plot 
lines3D(x = c(0,10), y = c(0,10), z = c(0,10),
        lwd = 3, bty = 'g', colkey = F,
        xlim=c(-5, 10), ylim=c(-2, 10), zlim=c(-5, 10), ticktype = 'detailed',
        theta = 0, phi = 30, #can change view if we want to
        xlab = 'n1', ylab = 'n2', zlab = 'n3')

#Plot the data vectors
#rep(0,2) -> [1] 0 0 : creates coordinates (0;0)
#x/y/z0: starting point (centered at origin)
#x/y/z1: end point (X matrix)
arrows3D(x0 = rep(0, 2), y0 = rep(0, 2), z0 = rep(0, 2), x1 = X[1,], y1 = X[2,], z1 = X[3,], 
         lwd = 2, col = c(2, 2), add = T, colkey = F, type = 'cone')

#Add labels to the arrows
text3D(x = X[1,], y = X[2,], z = X[3,], c(expression(X[1]), expression(X[2])), add = T)

#Add the deviation vectors
#Starting point: xbar = (5;2)
#Ending point: X (joins up with X vectors) 
arrows3D(x0 = xbar, y0 = xbar, z0 = xbar, x1 = X[1,], y1 = X[2,], z1 = X[3,], 
         lwd = 2, col = c(3, 3), add = T, colkey = F, type = 'cone')

text3D(xbar, xbar, xbar, c(expression(d[1]), expression(d[2])), add = T)
# dev.off()


#Question 3
arrows3D(x0 = rep(0, 2), y0 = rep(0, 2), z0 = rep(0, 2), x1 = Xc[1,], y1 = Xc[2,], z1 = Xc[3,], 
         lwd = 2, colvar = c(1,2), colkey = FALSE,
         xlim=c(-2, 5), ylim=c(-2, 5), zlim=c(-5, 2), ticktype = 'detailed',
         xlab = 'n1', ylab = 'n2', zlab = 'n3', theta = -30, phi = 0)

text3D(-0.5, -0.5, -0.5, expression(theta[12]), add = T)
text3D(x = Xc[1, ], y = Xc[2, ],z = Xc[3, ], c(expression(d[1]), expression(d[2])), add = T)

################################################################################

###################################Exercise 2###################################
#Question 1
xbar <- matrix(c(0.766,0.508,0.438,0.161))
S <- matrix(c(0.856,0.635,0.173,0.096,0.635,0.568,0.128,0.067,0.173,0.128,0.171,0.039,0.096,0.067,0.039,0.043), nrow = 4)

b <- matrix(c(1,1,1,1))

samp_mean <- t(b)%*%xbar
samp_mean

samp_var <- t(b)%*%S%*%b
samp_var


#Question 2
c <- matrix(c(1,-1,0,0))

samp_mean <- t(c)%*%xbar
samp_mean

samp_var <- t(c)%*%S%*%c
samp_var

samp_cov <- t(b)%*%S%*%c
samp_cov

################################################################################