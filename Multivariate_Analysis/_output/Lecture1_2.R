# Multivariate Honours

# Chapter 1

# 1.5 Exploratory Plots for Multivariate Data

# Scatterplot matrices

JW.T1.2 <- read.table ('T1-2.txt') #From Johnson & Wichern (2007), p. 15

dimnames(JW.T1.2) <- list (1:nrow(JW.T1.2), c('Density', 'Strength(MD)', 'Strength(CD)'))

# The 'older', manual way

pairs(JW.T1.2, pch = 16) #a matric of scatterplots is produced 

panel.boxplot <- function(x, ...)
{
  usr <- par('usr')
  on.exit(par(usr))
  par(usr = c(0, 4, usr[3:4]))
  box.out <- boxplot(x, plot = F)
  rect(1, box.out$stats[2], 3, box.out$stats[4])
  lines(c(1.5, 2.5), rep(box.out$stats[1], 2))
  lines(c(1.5, 2.5), rep(box.out$stats[5], 2))
  lines(c(1,3), rep(box.out$stats[3], 2), lwd = 2)
  lines(rep(2,2), box.out$stats[1:2])
  lines(rep(2,2), box.out$stats[4:5])
  points(rep(2, length(box.out$out)), box.out$out, pch = 16, col = 'red')
}

pairs(JW.T1.2, pch = 16, diag.panel = panel.boxplot, labels = c('', '', ''))

# Try experimenting with the parameters xaxt, yaxt and the functions mtext and text 
# to add the row and column identifications as well as the Min, Max and Med to exactly 
# reproduce the figure on p. 15


# Using GGally
library(GGally)

my.box.diag <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) +
    geom_boxplot(...) + 
    coord_flip()
}

ggpairs(JW.T1.2, 
        diag = list(continuous = my.box.diag),
        upper = list(continuous = 'smooth')
        ) + 
  theme_bw()


#Exploring some relationships:
panel.lin <- function (x, y){ 
  lines (x[order(x)], fitted(lm(y[order(x)]~x[order(x)])), col='green') 
  points (x,y) }
pairs(JW.T1.2[,-1], lower.panel=panel.smooth, upper.panel=panel.lin)


# There's always another way:
library(lattice)
lizard <- read.csv('Lizard.csv', header = T)
splom(lizard)

library(car)
scatterplotMatrix(~ SVL + HLS + Mass, data = lizard, regLine = list(col = 'green', lwd = 1), col = 'black',
                  smooth = list(col.smooth = 'red', lty.smooth = 1, col.spread = 'red', lty.spread = 2))

#Looking at correlations:
track <- read.csv('NationalTrack.csv', header = T)
head(track)
(R <- cor(track[,2:6]))

library(corrplot)
corrplot(R, method='circle')
corrplot(R, method='number')


AirPollution<-read.csv('Air Pollution Data.csv', header=T)
head(AirPollution)

(AirPCov <- cov(AirPollution))
(AirPCor <- cor(AirPollution))
corrplot(AirPCor,method='circle')
corrplot(AirPCor,method='number')


library(ggplot2)
library(ggcorrplot)
ggcorrplot(AirPCor)
ggcorrplot(AirPCor, 
           hc.order = TRUE, 
           type = 'lower',
           lab = TRUE)

#Here is a very quick way of visualising the 3-dimensional shape of your data:
library(rgl)

JW.T1.3 <- read.table('T1-3.txt')

open3d()
spheres3d (JW.T1.3$V1, JW.T1.3$V2, JW.T1.3$V3, col='blue')
axes3d()
