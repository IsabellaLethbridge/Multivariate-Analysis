# Multivariate Analysis Section B: Correspondence Analysis
# 10 June 2026
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Question 1
#-------------------------------------------------------------------------------
# Setup
ratings = structure(c(50,30,10,1,60,80,40,2,40,60,20,1,10,30,50,4),
                    dim = c(4L,4L),
                    dimnames = list(
                      c("High School","Bachelor's","Master's","Doctorate"),
                      c("Action","Drama","Comedy","Documentary"))
)
ratings

# 1
# Calculate row and column profiles:
P = ratings/sum(ratings)

# Row totals:
rm = apply(P,1,sum)
Dr = diag(rm)

# Column totals: 
cm = apply(P,2,sum)
Dc = diag(cm)

# Row profiles
rp = diag(1/rm)%*%P
rp
cp = diag(1/cm)%*%t(P)
cp

# 2
# Compute chi-squared distance 
weighted_rp = rp%*%diag(sqrt(1/cm))
as.matrix(dist(weighted_rp))

weighted_cp = cp%*%diag(sqrt(1/rm))
as.matrix(dist(weighted_cp))

# 3
S = diag(sqrt(1/rm))%*%as.matrix(P-rm%*%t(cm))%*%diag(sqrt(1/cm))
S = round(S,2)
colnames(S) = colnames(P)
rownames(S) = rownames(P)
S

# High School and Action
# High School and Documentary
# Master's and Action
# Master's and Documentary

# 4
svdS = svd(S)

# Principal row coordinates:
PC_F = diag(sqrt(1/rm))%*%svdS$u%*%diag(sqrt(svdS$d))
PC_F

# Principal column coordinates:
PC_G = diag(sqrt(1/cm))%*%svdS$v%*%diag(sqrt(svdS$d))
PC_G

# Plot
plot(PC_F[,1],PC_F[,2],col='deeppink',pch=16)
text(PC_F[,1],PC_F[,2],names(rm),pos=3)
points(PC_G[,1],PC_G[,2],col='steelblue2',pch=17)
text(PC_G[,1],PC_G[,2],names(cm),pos=3)

prop_var = (svdS$d[1:2])^2/sum(svdS$d^2)
prop_var

# Dimension 1 is responsible for 88.7% of the inertia. 
# Dimension 2 is responsible for 12% of the inertia.
# Because this is p-p cannot do row vs col

# 5
library(ca)
summary(ca(ratings))

# Dimension 1 and 2 account for 99.9% of the inertia, thus a 2D plot is an 
# accurate representation if the data 

# 6
pca_fit = prcomp(ratings)
biplot(pca_fit)
# PCA treats doctoral students as weird outliers for having low counts in each
# category.
# Uses Euclidean distance doesn't account for row and column masses differeing.
# Find row-col relationships.

# 7
plot(ca(ratings),
     mass=TRUE,
     arrows=c(TRUE,TRUE),
     map='rowgreen',
     contrib='absolute')

# 8
# When columns have highly unequal masses a principal row plot would be 
# difficult to interpret.
# Because coordinates are standardised it does not take into account if a 
# column is rare, this will be pulled extremely far from the origin. 
