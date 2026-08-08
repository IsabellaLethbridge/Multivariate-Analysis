dat = read.csv("bear_weights_lengths.csv")
dat = dat[-1]
length = dat[,c(5,6,7,8)]
xbar = apply(length, 2, mean)
S = var(length)
n = nrow(length)
q = ncol(length)
alpha = 0.05


(C1 = cbind(1, diag(-1, q-1)))

c2_a <- ((n - 1) * q) / (n - q) * qf(1 - alpha, q, n - q)

lower = xbar - sqrt(c2_a) * sqrt(diag(S) / n)
upper = xbar + sqrt(c2_a) * sqrt(diag(S) / n)

CI_a <- data.frame(
  Lower = round(lower, 4),
  Upper = round(upper, 4)
)

CI_a

# Question 2
C <- rbind(
  c(-1,  1,  0,  0),
  c( 0, -1,  1,  0),
  c( 0,  0, -1,  1)
)
# C <- rbind(
#   c(0,  0,  -1,  1),
#   c( 0, -1,  1,  0),
#   c( -1,  1, 0,  0)
# )

xbar_2 = C %*% xbar
S_2 = C %*% S %*% t(C)
q = nrow(C)

#T2 = n * t(xbar_2) %*% S_2 %*% xbar_2
c2_b <- ((n - 1) * q) / (n - q) * qf(1 - alpha, q, n - q)
lower_b <- xbar_2 - sqrt(c2_b) * sqrt(diag(S_2) / n)
upper_b <- xbar_2 + sqrt(c2_b) * sqrt(diag(S_2) / n)

CI_b <- data.frame(
  Variable = c("Lngth3 - Lngth2", "Lngth4 - Lngth3", "Lngth5 - Lngth4"),
  Lower = round(lower_b, 3),
  Upper = round(upper_b, 3)
)

CI_b

# Question 3

C <- rbind(
  c(-1,  1,  0,  0),
  c( 0, 0,  -1,  1)
)

xbar_2 = C %*% xbar
S_2 = C %*% S %*% t(C)
q = 2

#T2 = n * t(xbar_2) %*% S_2 %*% xbar_2
c3 <- ((n - 1) * q) / (n - q) * qf(1 - alpha, q, n - q)

eig <- eigen(S_2)
eig
axis1 <- sqrt(eig$values[1]) * sqrt(c3 / n)
axis2 <- sqrt(eig$values[2]) * sqrt(c3 / n)

phi <- atan2(eig$vectors[2, 1], eig$vectors[1, 1])

t <- seq(0, 2*pi, length.out = 500)

xx <- xbar_2[1] + axis1*cos(t)*cos(phi) - axis2*sin(t)*sin(phi)
yy <- xbar_2[2] + axis1*cos(t)*sin(phi) + axis2*sin(t)*cos(phi)

plot(xx, yy, type = "l", lwd = 2,
     xlab = expression(mu[Lngth3-Lngth2]),
     ylab = expression(mu[Lngth5-Lngth4]),
     main = expression("95% " * T^2 * " Confidence Ellipse"))
points(xbar_2[1], xbar_2[2], pch = 16)

# Optional: draw major/minor axes
mjrx <- xbar_2[1] + c(-1, 1) * axis1 * cos(phi)
mjry <- xbar_2[2] + c(-1, 1) * axis1 * sin(phi)
mnrx <- xbar_2[1] + c(-1, 1) * axis2 * cos(phi + pi/2)
mnry <- xbar_2[2] + c(-1, 1) * axis2 * sin(phi + pi/2)

lines(mjrx, mjry, lwd = 2, lty = 2)
lines(mnrx, mnry, lwd = 2, lty = 2)

# Question 4

m <- 7
tcrit <- qt(1 - alpha / (2 * m), df = n - 1)

C <- rbind(
  c(-1,  1,  0,  0),
  c( 0, -1,  1,  0),
  c( 0,  0, -1,  1)
)

xbar_inc <- as.numeric(C %*% xbar)
S_inc <- C %*% S %*% t(C)
q <- nrow(C)

# Means
bon_lower_means <- xbar - tcrit * sqrt(diag(S) / n)
bon_upper_means <- xbar + tcrit * sqrt(diag(S) / n)

Bon_means <- data.frame(
  Lower = round(bon_lower_means, 3),
  Upper = round(bon_upper_means, 3)
)

# Increases
bon_lower_inc <- xbar_inc - tcrit * sqrt(diag(S_inc) / n)
bon_upper_inc <- xbar_inc + tcrit * sqrt(diag(S_inc) / n)

Bon_inc <- data.frame(
  Variable = c("Lngth3 - Lngth2", "Lngth4 - Lngth3", "Lngth5 - Lngth4"),
  Lower = round(bon_lower_inc, 3),
  Upper = round(bon_upper_inc, 3)
)

Bon_means
Bon_inc

# Question 5

plot(xx, yy, type = "l", lwd = 2,
     xlab = expression(mu[Lngth3-Lngth2]),
     ylab = expression(mu[Lngth5-Lngth4]),
     main = expression("Ellipse and Bonferroni Rectangle"))
points(xbar_2[1], xbar_2[2], pch = 16)

rect(
  xleft   = bon_lower_inc[1],
  xright  = bon_upper_inc[1],
  ybottom = bon_lower_inc[3],
  ytop    = bon_upper_inc[3],
  border = "red",
  lwd = 2
)