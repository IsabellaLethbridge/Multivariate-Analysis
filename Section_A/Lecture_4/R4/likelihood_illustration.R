library(latex2exp) #For latex syntax in plot text

n <- 10

mus <- seq(-2, 2, length = 1e4)

x <- rnorm(n)
xbar <- mean(x)
L <- vector(length = length(mus))

for (i in 1:length(mus)) L[i] <- prod(dnorm(x, mean = mus[i]))

par(mfrow = c(1, 2))

plot(mus, L, type = 'l', lwd = 2)
points(mus[which.max(L)], max(L), pch = 16, cex = 2)
abline(v = xbar, lty = 2, lwd = 2, col = 'red')
mtext(text = bquote(bar(x) == .(round(xbar, 3))), 
      side = 1,
      cex = 1,
      line = 0.1,
      at = xbar)

muhat <- mus[which.max(log(L))]
plot(mus, log(L), type = 'l', lwd = 2)
points(muhat, max(log(L)), pch = 16, cex = 2)
abline(v = xbar, lty = 2, lwd = 2, col = 'red')
mtext(text = bquote(hat(mu) == .(round(muhat, 3))), 
      side = 1,
      cex = 1,
      line = 0.25,
      at = xbar)

round(xbar, 3)
round(mus[which.max(L)], 3)
