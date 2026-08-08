# Multivariate Analysis Section B: Correspondence Analysis 
# 21 July 2026 
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Setup
ratings = structure(
  c(
    50, 30, 10, 1, 60, 80, 40, 2,
    40, 60, 20, 1, 10, 30, 50, 4),
  dim = c(4L, 4L),
  dimnames = list(
    c("High School", "Bachelor's", "Master's", "Doctorate"),
    c("Action", "Drama", "Comedy", "Documentary"))
)

# Question 1
# Calculate row and column profiles:
P = ratings/sum(ratings)

r = apply(P,1,sum)
c = apply(P,2,sum)

Dr = diag(r)
Dc = diag(c)

rp = solve(Dr)%*%P
cp = solve(Dc)%*%t(P)

round(rp,4)
round(cp,4)

# Question 2
# Compute chi-squared distance between row and column profiles:
weighted_rp = rp%*%diag(1/sqrt(c))
weighted_cp = cp%*%diag(1/sqrt(r))

dist_r = as.matrix(dist(weighted_rp))
dist_c = as.matrix(dist(weighted_cp))

rownames(dist_r) = colnames(cp)
colnames(dist_r) = colnames(cp)
rownames(dist_c) = colnames(rp)
colnames(dist_c) = colnames(rp)

round(dist_r,2)
round(dist_c,2)

# Question 3
P_hat = P - r%*%t(c)
S     = solve(sqrt(Dr))%*%(P_hat)%*%solve(sqrt(Dc))

rownames(S) = rownames(P)
colnames(S) = colnames(P)

round(S,4)

# High School + Action
# High School + Documentary

# Question 4
# Calculate principal row and principal column coordinates:
svdS   = svd(S)
U      = as.matrix(svdS$u)
V      = as.matrix(svdS$v)
lambda = diag(svdS$d)

Fr = solve(sqrt(Dr))%*%U%*%lambda
Gc = solve(sqrt(Dc))%*%V%*%lambda

rownames(Fr) = rownames(P)
rownames(Gc) = colnames(P)

round(Fr,4)
round(Gc,4)

Fr_coords = Fr[,1:2]
Gc_coords = Gc[,1:2]

plot(rbind(Fr_coords,Gc_coords))

# Question 5
summary(ca(ratings))

# Dimension 1 represents approximately 88% of the total inertia.
# High School, Bachelors and Masters are perfectly represented in the first 2 
# dimensions, as well as Action and Documentary.
# Doctorate, Drama and Comedy are represented well in the first 2 dimensions 
# with a small portion being captured by the third dimension. 
# Dimension 1 explains majority of High School, Masters and Doctorate, while 
# dimension 2 explains majority of Bachelors. 
# Dimension 1 explains majority of Action and Documentary with Drama and Comedy
# split evenly between dimensions 1 and 2.

# Question 6
# Constrcut PCA biplot:
pca_fit = prcomp(ratings,scale.=TRUE,center=TRUE)
biplot(pca_fit)

# PCA doesn't account for differing number of observations and treats 
# doctorates as outliers due to the small counts in each category.

# Question 7
# Construct a CA plot for someone purely interest in relationship between 
# education levels (rows)
plot(ca(ratings),
     contrib='absolute',
     mass=TRUE,
     map='rowprincipal',
     arrows=c(TRUE,TRUE))

# Question 8
# A row principal biplot would be difficult to interpret visually when there 
# low inertia values.

