# Multivariate Section B: Principal Component Analysis
# 19 May 2026
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

#-------------------------------------------------------------------------------
# Question 1
# Plot family vs dist_rd:
ggplot(data = data_tidy_mali,
       aes(x = family, y = dist_rd))+
  geom_point(colour = "deeppink")+
  labs(
    title = "family vs dist_rd",
    x = "family",
    y = "dist_rd"
  )+
  theme_minimal()

# Outliers = dist_rd > 200 or family > 100:
data_tidy_mali <- data_tidy_mali |>
  filter(family < 100 & dist_rd < 400)

# Plot dist_rd vs cattle:
ggplot(data = data_tidy_mali,
       aes(x = dist_rd, y = cattle))+
  geom_point(colour = "deeppink")+
  labs(
    title = "dist_rd vs cattle",
    x = "dist_rd",
    y = "cattle"
  )+
  theme_minimal()

# Outliers = cattle > 100:
data_tidy_mali <- data_tidy_mali |>
  filter(cattle < 100)

#-------------------------------------------------------------------------------
# Question 2:
# Obtain variance-covariance matrix:
varcov_mat <- cov(data_tidy_mali)

# Variables are measured on different scales, thus it is important to 
# standardise the data to prevent components from being dominated by
# specific variables with larger variances.
# !!! Mention specific variables !!!

#-------------------------------------------------------------------------------
# Question 3:
# Standardise data:
dat_scaled <- data_tidy_mali |>
  scale() |>
  as.data.frame()

# Obtain correlation matrix:
cor_mat <- cov(dat_scaled)

# Eigen decomposition on correlation matrix:
eig <- eigen(cor_mat)
eig_vec_mat <- eig$vectors |>
  signif(2)

# Obtain scores:
loadings <- as.data.frame(eig$vectors) |>
  signif(2)
rownames(loadings) <- colnames(dat_scaled)
colnames(loadings) <- paste0("PC", 1:ncol(loadings))
scores <- as.matrix(dat_scaled)%*%as.matrix(loadings) |>
  as.data.frame()
head(scores)

# Obtain PCs:
PCs <- diag((t(eig_vec_mat))%*%cor_mat%*%eig_vec_mat)

# Plot scree plot:
index <- as.matrix(seq(1,9), ncol = 1)
PC_dat <- as.data.frame(cbind(PCs, index))

ggplot(data = PC_dat,
       aes(x = index, y = PCs))+
  geom_point(colour = "deeppink")+
  geom_line(colour = "deeppink")+
  labs(
    title = "Scree plot.",
    x = "Index",
    y = "Variance"
  )+
  theme_minimal()

# If you look at the elbow of the plot, 2 components effectively summarises
# the variability in the data.

#-------------------------------------------------------------------------------
# Question 4:
# Perform PCA using SVD:
# Data needs to be a matrix:
dat_mat <- as.matrix(dat_scaled)
svd_dat <- svd(dat_mat)

# Eigenvalues of covariance matrix are squares of singular values:
svd_dat$d^2 |> 
  signif(3)

# Right-singular vectors are the eigenvectors:
# Loadings:
svd_dat$v |>
  signif(2)

# Calculate scores: 
dat_mat%*%svd_dat$v |>
  head() |>
  signif(2)

#-------------------------------------------------------------------------------
# Question 5:
# PC1: 
# family, cotton, maize, bull load strongly onto PC1.
# areas with higher number of occupants.

# PC2: 
# dist_rd, sorg, millet load strongly onto PC2.
# sparsely populated areas. 

#-------------------------------------------------------------------------------
# Question 6:
# Calculate correlations between PC1 and variables:
# Eigenvectors:
P <- eig$vectors

# Eigenvalues:
D <- diag(eig$values)

# Correlation matrix = eigenvectors%*%sqrt(eigenvalues):
corr_mat <- P%*%sqrt(D)
rownames(corr_mat) <- paste0("V", 1:9)
colnames(corr_mat) <- paste0("PC", 1:9)

# Isolate PC:
col1 <- as.data.frame(corr_mat[,1, drop = FALSE])
colnames(col1) <- c("PC1")
col1 |> signif(2) |> knitr::kable()

#-------------------------------------------------------------------------------
# Question 7:
# Biplot: 
pca_res <- prcomp(data_tidy_mali, scale. = TRUE)
biplot(pca_res)
