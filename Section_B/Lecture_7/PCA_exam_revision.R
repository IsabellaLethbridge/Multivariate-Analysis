# Multivariate Section B: Principal Component Analysis
# 7 June 2026
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Setup
library(tidyverse)
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
# be sure to install from `2024` branch (code below does this)
remotes::install_github("MiguelRodo/DataTidy23RodoHonsMult@2024")
data("data_tidy_mali", package = "DataTidy23RodoHonsMult")
View(data_tidy_mali)
dat = data_tidy_mali

data("data_tidy_lum_response", package = "DataTidy23RodoHonsMult")
#-------------------------------------------------------------------------------
# Question 1
# Plot family vs distrd:
plot(dat$family,dat$dist_rd,
     xlab="Family",
     ylab="Distance to road",
     col="deeppink",
     cex=1.5)

# Plot distrd vs cattle
plot(dat$dist_rd,dat$cattle,
     xlab="Distance to road",
     ylab="Total cattle",
     col="deeppink",
     cex=1.5)

# Remove outliers:
dat = dat|>
  filter(family<=100 & dist_rd<=400)
dat = dat|>
  filter(cattle<=60)

# Question 2
# Standardize variables when units are on different scales.
dat = scale(dat)|>
  as.data.frame()

# Question 3
# Obtain correlation matrix:
cor_mat = cor(dat)

# Eigen decomposition:
eig_mat = eigen(cor_mat)
eig_vec = eig_mat$vectors
colnames(eig_vec) = paste0("PC",seq(1,9))
rownames(eig_vec) = colnames(dat)
signif(eig_vec,2)
eig_val = eig_mat$values|>
  signif(2)
prop_var = eig_val/sum(eig_val)
pc_index = seq(1,9)

plot(pc_index,prop_var,
     type='b',
     col="deeppink",
     xlab="Principal component",
     ylab="Proportion of variance explained")

# Calculate scores:
scores = as.matrix(dat)%*%eig_vec
scores1 = scores[,1]
scores2 = scores[,2]
scores1;scores2

# Chose 2 PCs, where the plot elbows.
# An additional PC won't explaining sufficiently more variance to warrant the 
# complexity of adding it. 

# Question 4
# Perform SVD:
X = svd(dat)
D = diag(X$d,nrow=9,ncol=9)
V = X$v # these are the loadings 
U = X$u

# Calculate scores (option 1):
scores = as.matrix(dat)%*%V
scores1 = scores[,1]
scores2 = scores[,2]
scores1;scores2

# Calculate scores (option 2):
scores = U%*%D
scores1 = scores[,1]
scores2 = scores[,2]
scores1;scores2

# Question 5
# PC1: family,cotton,bull load strongly
# PC2: dist_rd,sorg,millet load strongly 

# Question 6
# Calculate correlations using eigen decomposition:
correlations = (eig_mat$vectors)%*%sqrt(diag(eig_mat$values))

# Calculate correlations using scores and data: 
corr = cor(scores,dat)

# Question 7:
pca_res = prcomp(dat)
biplot(pca_res)
