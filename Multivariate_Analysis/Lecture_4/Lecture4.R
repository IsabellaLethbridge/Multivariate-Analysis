# Multivariate Honours 

# Chapter 4 - Testing multivariate normality

# J&W Example 4.14

T4_3 <- read.table("C:/Users/isabe/OneDrive - University of Cape Town/Documents/GitHub/Multivariate-Analysis/Multivariate_Analysis/Lecture_4/T4-3.DAT")

stiffness <- T4_3[, -5] #exclude 5th column 
S <- cov(stiffness)
n <- nrow(stiffness)

#1. Calculate squared distances
d2 <- mahalanobis(stiffness, colMeans(stiffness), cov = S) #Compare with T4_3[, 5]
T4_3[,5]

#2. Order the squared distances
d2_ord <- sort(d2)

#3. Graph the pairs
qcp <- qchisq((1:n - 0.5)/n, ncol(stiffness))
plot(qcp, d2_ord, main="Chi-square plot", pch = 16, 
     ylab = '')
title(ylab = expression(d[(j)]^2), line = 2)
abline(a = 0, b = 1, col = 'red')

