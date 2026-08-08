bearDat <- new.env()
load("J_WEx5.9.Rdata", envir = bearDat)

xbar = as.numeric(bearDat$xbar)
S = bearDat$S
n = 61
p = 6
alpha = as.numeric(0.05)
mu0 = c(95, 160, 55, 93, 18, 31)
names(xbar) <- c("Weight", "Body_length", "Neck",
                 "Girth", "Head_length", "Head_width")

dimnames(S) <- list(names(xbar), names(xbar))
View(xbar)
View(S)

(T2 <- n*t(xbar - mu0)%*%solve(S)%*%(xbar - mu0))
(1 - pf(T2*(n-p)/((n-1)*p), p, n-p))
Wilks = (1 + (T2 / (n - 1)))^-1
Wilks

c2 = ((n - 1) * p ) / (n - p) * qf((1 - alpha), p, n - p)
simul_lower = xbar - sqrt(c2) * sqrt(diag(S) / n)
simul_upper = xbar + sqrt(c2) * sqrt(diag(S) / n)

simul_CI = data.frame(
  Variable = names(xbar),
  Lower = round(simul_lower, 3),
  Upper = round(simul_upper, 3)
)

View(simul_CI)

c2chi = qchisq((1 - alpha), p)
simul_lower_chi = xbar - sqrt(c2chi) * sqrt(diag(S) / n)
simul_upper_chi = xbar + sqrt(c2chi) * sqrt(diag(S) / n)

simul_CI_chi = data.frame(
  Variable = names(xbar),
  Lower = round(simul_lower_chi, 3),
  Upper = round(simul_upper_chi, 3)
)

View(simul_CI_chi)

# Question 2

xbar_2 = xbar[c("Weight", "Girth")]
S_2 = S[c("Weight", "Girth"), c("Weight", "Girth")]
p2 = 2

c2_2 = ((n - 1) * p2) / (n - p2) * qf((1 - alpha), p2, n - p2)

eig = eigen(S_2)
eig
axis1 <- sqrt(eig$values[1]) * sqrt(c2_2 / n)
axis2 <- sqrt(eig$values[2]) * sqrt(c2_2 / n)

axis1 ; axis2 # major and minor

phi <- atan2(eig$vectors[2, 1], eig$vectors[1, 1])

t <- seq(0, 2*pi, length.out = 500)
xx <- xbar_2[1] + axis1*cos(t)*cos(phi) - axis2*sin(t)*sin(phi)
yy <- xbar_2[2] + axis1*cos(t)*sin(phi) + axis2*sin(t)*cos(phi)

plot(xx, yy, type = "l", lwd = 2,
     xlab = expression(mu[Weight]),
     ylab = expression(mu[Neck~girth]),
     main = "95% Confidence Ellipse")
points(xbar_2[1], xbar_2[2], pch = 16)

# Question 3

m = 6
tcrit = qt(1 - alpha / (2 * m), n - 1)

bon_lower = xbar - tcrit * sqrt(diag(S) / n)
bon_upper = xbar + tcrit * sqrt(diag(S) / n)

bon_CI <- data.frame(
  Variable = names(xbar),
  Lower = round(bon_lower, 3),
  Upper = round(bon_upper, 3)
)
View(bon_CI)

p = 6
c2 = ((n - 1) * p) / (n - p) * qf(1 - alpha, p, n - p)

eig = eigen(S)

half_lengths = sqrt(eig$values) * sqrt(c2 / n)

longest = max(half_lengths)
shortest = min(half_lengths)

round(longest, 3)
round(shortest, 3)
  
# Question 4
plot(xx, yy, type = "l", lwd = 2,
     xlab = expression(mu[Weight]),
     ylab = expression(mu[Neck~girth]),
     main = "Ellipse and Bonferroni Rectangle")
points(xbar_2[1], xbar_2[2], pch = 16)

rect(
  xleft   = bon_CI$Lower[bon_CI$Variable == "Weight"],
  xright  = bon_CI$Upper[bon_CI$Variable == "Weight"],
  ybottom = bon_CI$Lower[bon_CI$Variable == "Girth"],
  ytop    = bon_CI$Upper[bon_CI$Variable == "Girth"],
  border = "red", lwd = 2
)
  
# Question 5

m = 7
a = c(0, 0, 0, 0, -1, 1)

est = sum(a * xbar)
se = sqrt((t(a) %*% S %*% a) / n)
tcrit = qt(1 - alpha / (2 * m), n - 1)

lower7 = est - tcrit * se
upper7 = est + tcrit * se

est
ans = c(Lower = lower7, Upper = upper7)
ans