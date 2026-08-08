# Multivariate Analysis Assignment 11
# 13 May 2026
# Isabella Lethbridge and Josh Orelowitz

# Setup:
library(ggplot2)
library(reshape2)
library(ca)
library(factoextra)
library(dplyr)
library(tidyr)
pink_col <- c("#FF9FCC", "#D63384", "#B3126A", "#8B004B", "#6A003A", "#4A0028")

#-------------------------------------------------------------------------------
# Question 1:
ratings <- structure(
  c(
    50, 30, 10, 1, 60, 80, 40, 2,
    40, 60, 20, 1, 10, 30, 50, 4),
  dim = c(4L, 4L),
  dimnames = list(
    c("High School", "Bachelor's", "Master's", "Doctorate"),
    c("Action", "Drama", "Comedy", "Documentary"))
)

#-------------------------------------------------------------------------------
# (1)
# Correspondence matrix:
P <- ratings/sum(ratings)

# Row profiles:
r_vec <- apply(P,1,sum) #rowSums()
Dr    <- diag(r_vec)
rp    <- solve(Dr)%*%P

# Column profiles:
c_vec <- apply(P,2,sum) #colSums()
Dc    <- diag(c_vec)
cp    <- solve(Dc)%*%t(P)

#-------------------------------------------------------------------------------
# (2)
# Chi-squared distance between rows:
weighted_rp <- rp%*%diag(sqrt(1/c_vec))
dist_mat_r  <- as.matrix(dist(weighted_rp))

# Chi-squared distance between columns:
weighted_cp <- cp%*%diag(sqrt(1/r_vec))
dist_mat_c  <- as.matrix(dist(weighted_cp))

# Heat maps:
# Add row and column labels:
rownames(dist_mat_r) <- 
  colnames(dist_mat_r) <- 
  c("High School", "Bachelor's", "Master's", "Doctorate")
rownames(dist_mat_c) <- 
  colnames(dist_mat_c) <- 
  c("Action", "Drama", "Comedy", "Documentary")

# Row heatmap:
df_r <- melt(dist_mat_r)
row_dist <- ggplot(df_r,
                   aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2),size = 2)) +
  scale_fill_gradientn(colours = pink_col, name = "Distance") +
  labs(
    title = "Row distance heatmap.",
    x = "",
    y = "") +
  theme_minimal()

# Column heatmap:
df_c <- melt(dist_mat_c)
col_dist <- ggplot(df_c,
                   aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2), size = 2)) +
  scale_fill_gradientn(colours = pink_col, name = "Distance") +
  labs(
    title = "Column distance heatmap.",
    x = "",
    y = "") +
  theme_minimal()

# *insert comment*

#-------------------------------------------------------------------------------
# (3)
# Rows: Doctorate and High school > Master's and High School > Doctorate and Bachelors
# Columns: Documentary and Action > Documentary and Comedy > Documentary and Drama

# (4)
rc_mat <- r_vec%*%t(c_vec)
S      <- diag(sqrt(1/r_vec))%*%(as.matrix(P) - rc_mat)%*%diag(sqrt(1/c_vec))
svdS   <- svd(S)
lambda <- diag(svdS$d)

# Principal row coordinates:
F_mat <- diag(sqrt(1/r_vec))%*%svdS$u%*%lambda
rownames(F_mat) <- rownames(ratings)

# Principal column coordinates:
G_mat <- diag(sqrt(1/c_vec))%*%svdS$v%*%lambda
rownames(G_mat) <- colnames(ratings)

# Plot:
coord <- rbind(F_mat[,1:2], G_mat[,1:2])
xlims <- range(coord[,1]) * 1.2
ylims <- range(coord[,2]) * 1.2

plot(F_mat[,1], 
     F_mat[,2], 
     type = "n", 
     xlim = xlims, 
     ylim = ylims,
     main = "Principal coordinate plot.",
     xlab = paste0("Dimension 1 (", round((svdS$d[1]^2/sum(svdS$d^2))*100, 1), "%)"),
     ylab = paste0("Dimension 2 (", round((svdS$d[2]^2/sum(svdS$d^2))*100, 1), "%)"))
abline(h = 0, v = 0, lty = 2, col="gray") 

# Plot Rows (Education)
points(F_mat[,1], 
       F_mat[,2], 
       col = "steelblue", 
       pch = 19,
       cex = 1.5)
text(F_mat[,1], 
     F_mat[,2], 
     labels = rownames(F_mat), 
     col = "steelblue", 
     pos = 1,
     cex = 0.8)

# Plot Columns (Genres)
points(G_mat[,1], 
       G_mat[,2], 
       col = "deeppink", 
       pch = 17,
       cex = 1.5)
text(G_mat[,1], 
     G_mat[,2], 
     labels = rownames(G_mat), 
     col = "deeppink", 
     pos = 3,
     cex = 0.8)

legend("top", 
       legend = c("Education", "Genre"), 
       col = c("steelblue", "deeppink"), 
       pch = c(19, 17),
       cex = 1)

# *interpret results*

#-------------------------------------------------------------------------------
# (5)
summary(ca(ratings))

# First 2 dim account for 88.7 + 11.2 = 99.9% of (total) inertia 
# blah blah blah *inser interpretation* 

#-------------------------------------------------------------------------------
# (6)

#-------------------------------------------------------------------------------
# (7)
# Rows = principal
# Columns = standard
# Not sure which plot to use (?)
plot(ca(ratings), 
     mass = TRUE, 
     contrib = "absolute", 
     map = "rowprincipal", 
     arrows = c(TRUE, TRUE))

plot(ca(ratings), 
     mass = TRUE, 
     contrib = "absolute", 
     map = "rowgreen", 
     arrows = c(TRUE, TRUE))

#-------------------------------------------------------------------------------
# (8)
# *insert explanation* 

#-------------------------------------------------------------------------------
# Question 2:
data("HairEyeColor")

#-------------------------------------------------------------------------------
# (1)
mca_mat <- as.data.frame(HairEyeColor) |>
  uncount(Freq)

#-------------------------------------------------------------------------------
# (2)
summary(mjca(mca_mat, lambda = "adjusted"))
plot(mjca(mca_mat), 
     mass= TRUE, 
     contrib = "absolute", 
     map = "colprincipal", 
     arrows = c(TRUE, TRUE))

# First dim accounts for 65.6% of associations.
# First two dims account for 73.1% of total inertia. 
# Hair:Blond and Eye:Blue are associated (dim 1)
# Hair:Black and Eye:Brown are associated (dim 1)
# Sex is near origin (low contribution) 
# Dim 1: Lighter vs Darker features 
# Dim 2: Common vs Rare features 

# *interpret summary* 
