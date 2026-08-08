# Multivariate Honours

# Chapter 3

# Different ways of plotting Bivariate Normal Distributions

# 1. Using only base plotting functions (persp)
library(mvtnorm) 

x1 <- x2 <- seq(-4, 7, length = 50)

mu <- c(1, 2)
sigma <- matrix(c(1, 0.5,
                  0.5, 2), 2)

#mvtnorm and emdbook (used later) both have a function called dmvnorm. Make sure you specify the appropriate one!
dens <- matrix(mvtnorm::dmvnorm(expand.grid(x1, x2), 
                                mean = mu, sigma = sigma), ncol = length(x1))

persp(dens,
      theta = 80, phi = 30, expand = 0.6, #viewing parameters 
      shade = 0.25, col = 'lightblue', xlab = 'x1', ylab = 'x2', zlab = 'f(x1, x2)')


# 2. scatterplot3d (see-through lines)

library(scatterplot3d)
x1 <- x2 <- seq(-10, 10, length = 100)

mu <- c(0, 0)
sigma <- matrix(c(3, 1,
                  1, 3), 2)

dens <- matrix(mvtnorm::dmvnorm(expand.grid(x1, x2), 
                                mean = mu, sigma = sigma), 
               ncol = length(x1))

s3d <- scatterplot3d(x1, x2,
                     seq(min(dens), max(dens), length = length(x1)),
                     type = "n", grid = FALSE, angle = 80,
                     zlab = expression(f(x[1], x[2])),
                     xlab = expression(x[1]), ylab = expression(x[2]),
                     main = "Bivariate normal distribution") #This is just an empty plot with the appropriate dimensions

for(i in length(x1):1)
  s3d$points3d(rep(x1[i], length(x2)), x2, dens[i,], type = "l")
for(i in length(x2):1)
  s3d$points3d(x1, rep(x2[i], length(x1)), dens[,i], type = "l")
#Will only look sensible if made full screen/exported


# 3. emdbook (interactive plot)

library(emdbook)
curve3d(emdbook::dmvnorm(c(x,y), mu = c(0,0), Sigma=diag(2)), 
        sys3d = 'rgl', front = 'line', back = 'line', 
        xlim=c(-5,5), ylim=c(-5,5), zlab = "f(x, y)")


# 4. rgl (interactive plot)

library(rgl)

x <- y <- seq(-4, 4, 0.1)
z1 <- mvtnorm::dmvnorm(expand.grid(x, y), mean = c(0,0), sigma = diag(2)) #Independent
z2 <- mvtnorm::dmvnorm(expand.grid(x, y), mean = c(0,0), sigma = matrix(c(1, 0.75, 0.75, 1), 2)) #Cov(X1, X2) = 0.75

persp3d(x, y, z1, col = 'white', xlab = 'X1', ylab = 'X2', zlab = 'f(X1, X2)', zlim = c(0, 0.4), box = F)
rgl.viewpoint(0, -60)
surface3d(x, y, z1, front = 'lines')
par3d(windowRect = c(10,30, 900, 1000), zoom = 0.75) #Positioning and zoom

open3d() #Don't overwrite previous plot
persp3d(x, y, z2, col = 'white', xlab = 'X1', ylab = 'X2', zlab = 'f(X1, X2)', zlim = c(0, 0.4), box = F)
rgl.viewpoint(0, -60)
surface3d(x, y, z2, front = 'lines')
par3d(windowRect = c(910, 30, 1800, 1000), zoom = 0.75) #Positioning and zoom


# 5. manipulate (interactive parameters) together with rgl
# Click the cog wheel in the plot to access the parameters specified below
# This doesn't work that well with plots in an rgl window, rather see shiny application

library(manipulate)

x <- seq(-6, 6, 0.1)
y <- seq(-6, 6, 0.1)
xy <- expand.grid(x, y)
manipulate({
  z <- mvtnorm::dmvnorm(xy, mean = c(0,0), sigma = matrix(c(variance1, covariance, covariance, variance2), 2, 2))
  plot(c(0,1),c(0,1),type='n',main='Placeholder Plot')
  nbcol <- 150
  colour <- rev(rainbow(nbcol, start = 0, end = 0.7))
  zcol  <- cut(z, nbcol)
  persp3d(x, y, z, col = colour[zcol], xlab = 'X1', ylab = 'X2', zlab = 'f(X1, X2)')
  par3d(windowRect = c(8,30, 958, 1030), zoom = 0.8)
  legend3d('top', 'Bivariate Normal Distribution', cex = 3)
  # play3d(spin3d(axis = c(0, 0, -1), rpm = 12), duration = 5) # Uncomment this line to get a spinning plot with every change
}, 
variance1 = slider(1, 5, 1, 'Variance(X1)', 0.5), 
variance2 = slider(1, 5, 1, 'Variance(X2)', 0.5), 
covariance = slider(-0.9, 0.9, 0, 'Covariance(X1, X2)', 0.1))



## Adding contour lines to simulated data

library(MASS) # For mvrnorm function
mu <- c(0, 0)
sigma <- matrix(c(1, 0.9,
                  0.9, 1), 2)

set.seed(123)
X <- as.data.frame(mvrnorm(n = 10000, mu = mu, Sigma = sigma))
colnames(X) <-c('X1', 'X2')

library(ggplot2)
library(ggExtra)
theme_update(plot.title = element_text(hjust = 0.5, size = 16))

m <- ggplot(as.data.frame(X), aes(x = X1, y = X2)) + geom_point() 
mm <- m + geom_density2d() + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  ggtitle(expression(paste(sigma[11], ' = ', sigma[22], '= 1   ;   ', sigma[12], ' = 0.9')))
mm

## Adding marginal densities as well
ggExtra::ggMarginal(mm, type = 'density')



## Contours for specified distributions

# 1. Using eigenvectors and eigenvalues & Chi-square distribution
library(plotrix) # Just for white background in text labels
ellipse <- function(alpha, mu = c(0, 0), sigma = diag(2), new = T, my.col = 'black')
{
  for (i in 1:length(alpha)){
    alpha <- sort(alpha)
    
    eig <- eigen(sigma)
    l1 <- eig$values[1]
    l2 <- eig$values[2]
    phi1 <- atan2(eig$vectors[2, 1], eig$vectors[1, 1]) # angle of major axis
    a <- (l1 * qchisq(1 - alpha[i], 2))^0.5 # Major axis length
    b <- (l2 * qchisq(1 - alpha[i], 2))^0.5 # Minor axis length
    
    #ellipse points
    t <- seq(0, 2*pi, 0.01) 
    xx <- mu[1] + a*cos(t)*cos(phi1) - b*sin(t)*sin(phi1)
    yy <- mu[2] + a*cos(t)*sin(phi1) + b*sin(t)*cos(phi1)
    
    ## Plot ellipse
    xtrm <- max(abs(c(xx, yy))) + 1
    if (new && (i == 1)){
        plot(xx, yy, type = 'l', lwd = 2.5, xlab = expression(X[1]), ylab = expression(X[2]), 
              xlim = c(-xtrm, xtrm), ylim = c(-xtrm, xtrm), col = my.col)
          boxed.labels(xx[which.min(yy)], min(yy), substitute(bquote(alpha ~ '=' ~ al), list(al = alpha[i])),
                       border = F, bg = 'white', xpad = 0.55, ypad = 0.55)
    }
    else {
      lines(xx, yy, col = my.col, lwd = 2.5)
      boxed.labels(xx[which.min(yy)], min(yy), substitute(bquote(alpha ~ '=' ~ al), list(al = alpha[i])),
                   border = F, bg = 'white', xpad = 0.55, ypad = 0.55)
    }
  }
}

mu <- c(0, 0)
sigma <- diag(2)
alpha <- c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5)
ellipse(alpha, mu, sigma)


# 2. A bit fancier, but not calculated based on alpha:
library(rgl)
library(plot3Drgl)

x <- y <- seq(-4, 4, 0.1)
mu <- c(0, 0)
z1 <- mvtnorm::dmvnorm(expand.grid(x, y), mean = mu, sigma = diag(2))
z2 <- mvtnorm::dmvnorm(expand.grid(x, y), mean = mu, sigma = diag(2, 2))

#Sigma = I
open3d()
persp3d(x, y, z1, col = 'white', 
        xlab = 'X1', ylab = 'X2', zlab = 'f(X1, X2)', 
        zlim = c(0, 0.2), box = T, alpha = 0.5)
rgl.viewpoint(0, -80)
surface3d(x, y, z1, col = 'lightblue', alpha = 0.5)
par3d(windowRect = c(10, 30, 900, 1000), zoom = 0.75)

lines <- contourLines(x, y, matrix(z1, length(x)))
for (i in seq_along(lines)) {
  xx <- lines[[i]]$x
  yy <- lines[[i]]$y
  zz <- rep(lines[[i]]$level, length(x))
  lines3d(xx, yy, zz, lwd = 2)
  lines3d(xx, yy, 0, lwd = 2)
}

contour2Drgl(matrix(z1, length(x)), x, y,
             xlab = 'x1', ylab = 'x2')


#Sigma = 2*I
open3d()
persp3d(x, y, z2, col = 'white', 
        xlab = 'X1', ylab = 'X2', zlab = 'f(X1, X2)', 
        zlim = c(0, 0.2), box = T, alpha = 0.5)
rgl.viewpoint(0, -80)
surface3d(x, y, z2, col = 'lightblue', alpha = 0.5)
par3d(windowRect = c(10, 30, 900, 1000), zoom = 0.75)

lines <- contourLines(x, y, matrix(z2, length(x)))
for (i in seq_along(lines)) {
  xx <- lines[[i]]$x
  yy <- lines[[i]]$y
  zz <- rep(lines[[i]]$level, length(x))
  lines3d(xx, yy, zz, lwd = 2)
  lines3d(xx, yy, 0, lwd = 2)
}

contour2Drgl(matrix(z2, length(x)), x, y,
             xlab = 'x1', ylab = 'x2')
