# Multivariate Analysis Section B: Factor Analysis examples
# 25 May 2026
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Principal Component Method: Example 1
#-------------------------------------------------------------------------------
# Read in correlation matrix:
cor_mat1 <- matrix(c(1,0.02,0.96,0.42,0.01,0.02,1,0.13,0.71,0.85,0.96,0.13,
                    1,0.5,0.11,0.42,0.71,0.5,1,0.79,0.01,0.85,0.11,0.79,1), 
                  nrow = 5, 
                  ncol = 5, 
                  byrow = TRUE)

# Perform eigenvalue decomposition of correlation matrix:
eig_cor1 <- eigen(cor_mat1)
eig_val1 <- eig_cor1$values
eig_vec1 <- eig_cor1$vectors

# Factor loadings are scaled eigenvectors:
l1 <- eig_vec1[,1:2]%*%diag(sqrt(eig_val1[1:2]))

# Communalities = sum of squared loadings:
h1 <- diag(l1%*%t(l1))

# Calculate specific variances: 
psi1 <- 1-h1

# Create diagonal matrix of specific variances:
psi_mat1 <- diag(psi1)

# Re-create correlation matrix:
reproduced_cor_mat1 <- l1%*%t(l1)+psi_mat1

#-------------------------------------------------------------------------------
# Principal Component Method: Example 2
#-------------------------------------------------------------------------------
# Read in correlation matrix:
cor_mat2 <- matrix(c(1,0.632,0.511,0.115,0.155,
                    0.632,1,0.574,0.322,0.213,
                    0.511,0.574,1,0.183,0.146,
                    0.115,0.322,0.183,1,0.683,
                    0.155,0.213,0.146,0.683,1), 
                  nrow = 5,
                  ncol = 5,
                  byrow = TRUE)

# Perform eigenvalue decomposition:
eig_cor2 <- eigen(cor_mat2)
eig_val2 <- eig_cor2$values
eig_vec2 <- eig_cor2$vectors

# 1 factor:
# Calculate loadings:
l2_i <- eig_vec2[,1:1]*(sqrt(eig_val2[1]))
l2_i

# Calculate communialities:
h2_i <- diag(l2_i%*%t(l2_i))

# Calculate psi:
psi2_i <- 1-h2_i

# 2 factors:
# Calculate loadings:
l2_ii <- eig_vec2[,1:2]%*%(diag(sqrt(eig_val2[1:2])))
l2_ii

#  Calculate communalities:
h2_ii <- diag(l2_ii%*%t(l2_ii))

# Calcualte psi:
psi2_ii <- 1-h2_ii

#-------------------------------------------------------------------------------
# Maximum likelihood: Example 3
#-------------------------------------------------------------------------------
FA <- factanal(covmat = cor_mat2, factors=2, rotation = "none")
print(FA, cutoff=0)

# Rotation:
FA1 <- factanal(covmat = cor_mat2, factors=2, rotation = "none")
varimax(loadings(FA1))
