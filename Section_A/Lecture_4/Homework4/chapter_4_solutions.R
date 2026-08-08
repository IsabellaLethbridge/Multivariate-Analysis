rm(list = ls())

### Homework exercise 4.3

airpol <- read.csv('Air Pollution Data.csv')
n <- nrow(airpol) ; p <- ncol(airpol)

# 1
sol <- airpol$Radiation

# pdf('ex_4_3_a.pdf')
qqp <- qqnorm(sol, pch = 16) #creates QQ-plot 
qqline(sol) #Adds line
#Note that sample quantiles aren't standardised
# dev.off()

#Calculate the correlation between theoretical & empirical
round(cor(qqp$x, qqp$y), 4)

# 2
biv <- airpol[, c('NO2', 'O3')] #just look at these two variables
#Calculate the squared (Mahalanobis) distance for each data point
d2 <- mahalanobis(biv, colMeans(biv), cov = var(biv))
d2

# 3
#We would expect HALF of the distance below the median of ~chi^2_2
(chisq_med <- qchisq(0.5, 2))
#The proportion of distances below it:
mean(d2 < chisq_med)

# 4
#Create Chi-square plot
d_rank <- rank(d2)
q <- qchisq((d_rank-0.5)/n, 2)
# pdf('ex_4_3_b.pdf')
plot(d2 ~ q, main = 'Chi-square plot', pch = 16)
abline(a=0, b=1, col = 'red')
# dev.off()
