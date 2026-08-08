# Multivariate Analysis Section B: Principal Component Analysis 
# 2 July 2026 
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Setup
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
# be sure to install from `2024` branch (code below does this)
remotes::install_github("MiguelRodo/DataTidy23RodoHonsMult@2024")
data("data_tidy_mali", package = "DataTidy23RodoHonsMult")

# View(data_tidy_mali)
dat = data_tidy_mali

# Load required libraries:
library(dplyr)

#-------------------------------------------------------------------------------
# Question 1
# Scatterplot of family vs dist_rd:
plot(dat$family,dat$dist_rd)

# Scatterplot of distrd vs cattle
plot(dat$dist_rd,dat$cattle)

# Remove outliers:
dat = dat|>
  filter(family<=100)|>
  filter(dist_rd<=200)|>
  filter(cattle<=60)

#-------------------------------------------------------------------------------
# Question 2
# It is important to standardize variables when they are measured in different
# units. In this dataset dist_rd is measured in km, whereas millet is measured
# in hectares. This ensures a comparable analysis and prevents one large varable
# from dominating the principal components. 

#-------------------------------------------------------------------------------
# Question 3
# Correlation matrix: 
cor_mat = cor(dat)
cor_mat

# Eigen decomposition:
eig_mat = eigen(cor_mat)
eig_val = eig_mat$values
eig_val
eig_vec = eig_mat$vectors
eig_vec

# Variance explained (per component):
tot_var  = sum(eig_val)
prop_var = eig_val/tot_var
cum_var = cumsum(prop_var)
comp     = seq(1:length(eig_val))
plot(comp,prop_var,
     type='b')
abline(v=4,
       lty=2)

# Scores:
eig_vec = as.matrix(eig_vec)
pcs     = eig_vec[,1:4]
dat     = as.matrix(dat)
scores  = dat%*%pcs
scores

#-------------------------------------------------------------------------------
# Question 4
svd_mat = svd(cor_mat)
d       = svd_mat$d
d_sqr   = d^2
u       = svd_mat$u
v       = svd_mat$v
v
pcs     = -v[,1:4]
pcs
scores  = dat%*%pcs
scores

#-------------------------------------------------------------------------------
# Question 5
rownames(pcs) = colnames(data_tidy_mali)
pcs
# PC1: family,cotton,bull (subsistence farming) 
# PC2: dist_rd,sorg,millet (commercial farming)

#-------------------------------------------------------------------------------
# Question 6
corr = diag(sqrt(eig_val))%*%eig_vec
rownames(corr) = colnames(data_tidy_mali)
corr[,1]

# family,cotton,maize are most correlated with PC1
# There are generally greater loadings for more strongly correlated variables. 

#-------------------------------------------------------------------------------
# Question 7
dat = data_tidy_mali
dat = dat|>
  filter(family<=100)|>
  filter(dist_rd<=200)|>
  filter(cattle<=60)
pca = prcomp(dat,scale=TRUE)
biplot(pca)

# maize, cattle orthogonal to millet,sorg 
# family,cotton,bull,goats orthogonal to dist_rd 
# 50,29 large values for sorg,millet
# 54,55,43 average values for goats