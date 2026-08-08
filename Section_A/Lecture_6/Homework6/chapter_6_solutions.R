options(digits = 3)
options(show.signif.stars = F)

rm(list = ls())

library(xtable)
library(dplyr)

### Homework exercise 6.1

male <- read.table('T6-11.dat')
female <- read.table('T5-12.dat')
vars <- c('Tail', 'Wing')
colnames(male) <- colnames(female) <- vars

#a) Scatterplot

# pdf('6.1.a1.pdf')
plot(male, pch = 16)
# dev.off()

max(male$Tail)
male_adj <- male[-which.max(male$Tail),]
#OR male_adj <- subset(male, Tail < max(Tail))

# pdf('6.1.a2.pdf')
plot(male_adj, pch = 16)
# dev.off()

#b) MANOVA

male_adj <- cbind(male_adj, gender = 'male')
female <- cbind(female, gender = 'female')
all_birds <- rbind(male_adj, female)

male_means <- colMeans(male_adj[,1:2])
female_means <- colMeans(female[,1:2])

#The tidyverse way of summarising data:
all_means <- all_birds %>% 
  group_by(gender) %>% 
  summarise(across(where(is.numeric), ~mean(.x)))

#An example of creating a latex table with caption in R:
# print(xtable(all_means, caption = 'Mean tail and wing lengths for female and male hook-billed kites'), 
#       file = '6.1.b.tex', include.rownames = F)

birds_manova <- manova(cbind(Tail, Wing) ~ gender, data = all_birds)
bird_man_sum <- summary(birds_manova, 'Wilks')
#xtable supports aov and anova objects, but not manova
#Will need to make another plan to export nicely (try it!)

birds_anovas <- summary.aov(birds_manova)
#Separate ANOVA's:
# print(xtable(birds_anovas[1]), file = '6.1.b_tail.tex')
# print(xtable(birds_anovas[2]), file = '6.1.b_wing.tex')

#c) Bonferroni CI's

diff <- as.matrix(male_means - female_means)
colnames(diff) <- '$\\bm{\\bar{x}}_M - \\bm{\\bar{x}}_F$'
# print(xtable(diff), sanitize.text.function = function(x) {x}, file = '6.1.c.tex')
n1 <- nrow(male_adj)
n2 <- nrow(female)
n <- n1 + n2
g <- 2
p <- 2
alpha <- 0.05

W <- bird_man_sum$SS$Residuals

BonCI <- cbind(diff, diff) + 
  qt(1 - alpha/(p*g*(g-1)), n-g)*sqrt(diag(W)/(n-g)*(1/n1 + 1/n2))%*%t(c(-1, 1))
colnames(BonCI) <- c('Lower', 'Upper')

# print(xtable(BonCI), file = '6.1.c2.tex')



### Homework exercise 6.2
rm(list = ls())

crops <- read.table('T6-17.dat')

str(crops)
#V1 & V2 are integers!
crops$V1 <- as.factor(crops$V1)
crops$V2 <- as.factor(crops$V2)

colnames(crops) <- c('Location', 'Variety', 'Yield', 'Kernels', 'SeedSize')

#a) Two-way MANOVA

crop_fit <- manova(cbind(Yield, Kernels, SeedSize) ~ Location*Variety, data = crops)
crop_summary <- summary(crop_fit, test="Wilks")
crop_summary
#Interaction is quite significant!

#Run separate ANOVA's
crop_anovas <- summary.aov(crop_fit)
# print(xtable(crop_anovas[1]), file = '6.2.a1_tail.tex')
# print(xtable(crop_anovas[2]), file = '6.2.a2_tail.tex')
# print(xtable(crop_anovas[3]), file = '6.2.a3_tail.tex')


#Bonferroni intervals
library(DescTools)
PostHocTest(aov(Yield ~ Location*Variety, data = crops), method = 'bonferroni')
PostHocTest(aov(Kernels ~ Location*Variety, data = crops), method = 'bonferroni')
PostHocTest(aov(SeedSize ~ Location + Variety, data = crops), method = 'bonferroni')


#b) Testing residuals

res <- crop_fit$residuals

# pdf('6.2.b.pdf')
par(mfrow=c(2,2))
qqnorm(scale(res[, 1]), main = 'Yield')
abline(a = 0, b = 1, col = 'red')
qqnorm(scale(res[, 2]), main = 'Kernels')
abline(a = 0, b = 1, col = 'red')
qqnorm(scale(res[, 3]), main = 'SeedSize')
abline(a = 0, b = 1, col = 'red')

n <- nrow(res) ; p <- ncol(res)
d2 <- mahalanobis(res, colMeans(res), cov = cov(res)) 
d2_ord <- sort(d2)
qcp <- qchisq((1:n - 0.5)/n, p)
plot(qcp, d2_ord, main="Chi-square plot", ylab = '', xlim = c(0, 9), ylim = c(0, 6))
title(ylab = expression(d[(j)]^2), line = 2)
abline(a = 0, b = 1, col = 'red')
# dev.off()
