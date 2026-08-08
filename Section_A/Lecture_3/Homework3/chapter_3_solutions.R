### Homework exercise 3.2

# 2.

library(plotrix) # Just for white background on text labels
ellipse <- function(alpha, mu = c(0, 0), sigma = diag(2), new = T, my.col = 'black')
{
  for (i in 1:length(alpha)){
    alpha <- sort(alpha)
    
    eig <- eigen(sigma)
    l1 <- eig$values[1]
    l2 <- eig$values[2]
    phi1 <<- atan2(eig$vectors[2, 1], eig$vectors[1, 1]) # angle of major axis
    phi2 <<- atan2(eig$vectors[2, 2], eig$vectors[1, 2]) # angle of minor axis
    a <<- (l1 * qchisq(1 - alpha[i], 2))^0.5 # Major axis length
    b <<- (l2 * qchisq(1 - alpha[i], 2))^0.5 # Minor axis length
    
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

mu <- c(0, 2)
sig11 <- 2
sig22 <- 1
sig12 <- 0.5*sqrt(sig11)*sqrt(sig22)
sigma <- matrix(c(sig11, sig12, sig12, sig22), 2)
alpha <- 0.1


#Eigen pairs
eigen(sigma)


#Plot ellipse
# pdf('ex_3_2.pdf')
ellipse(alpha, mu, sigma)
abline(v = 0, h = 0)
points(mu[1], mu[2], pch = 16)
lines(c(0, a*cos(phi1)) + mu[1], c(0, a*sin(phi1)) + mu[2], col = 'red', lwd = 2)
lines(c(0, b*cos(phi2)) + mu[1], c(0, b*sin(phi2)) + mu[2], col = 'red', lwd = 2)
# dev.off()