# Question 1

dat = read.csv("Air Pollution Data.csv")
solar = dat$Radiation
n = length(solar)
devs = (solar - mean(solar))/ sd(solar)
ord_devs = sort(devs)
ord_solar = sort(solar)

p = (1:n - 0.5)/n
theory = qnorm(p)
# plot(theory, ord_solar, pch = 16)
# abline(lm(ord_solar ~ theory), col = "red", lwd = 2) # Check this out
plot(theory, ord_devs, pch = 16)
abline(a = 0, b = 1, col = "red") #what to do fr?
qqnorm(ord_devs, pch = 16)
abline(a = 0, b = 1, col = "red")
r = cor(theory, ord_solar)
r
n
#for a n of 40  the correspodning alpha levels to disprove normality are
#0.9599 - 0.01
#0.9726 - 0.05
#0.9771 - 0.10
# As our r value of 0.9693 is smaller than the 0.1 and 0.05 levels of significance
# but not bigger than the 0.01 significance level. 
# There is strong evidence against normality. This is further supported by
# the fact that the dots do not closely follow the straight line but rather
# They are sparse around the tails which is usually okay but the middle part
# Especially the early middle is too far above the line

# Question 2
dat2 = dat[, c("NO2", "O3")]

Xbar = colMeans(dat2)
S = cov(dat2)
n = nrow(dat2)

d2 = mahalanobis(dat2, Xbar, S)
d2

# Question 3
cutoff = qchisq(0.5, 2)
prop = mean(d2 <= cutoff)
prop

# Question 4
ord_d2 = sort(d2)
p = (1:n - 0.5)/n
theoryChi = qchisq(p, 2)
plot(theoryChi, ord_d2, pch = 16)
abline(a = 0, b = 1, col = "red")






