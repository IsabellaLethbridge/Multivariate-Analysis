# Multivariate Analysis Section B: LDA Exercises
# 7 June 2026
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Exercise 1
#-------------------------------------------------------------------------------
# Setup:
# Target observation:
x0 = matrix(c(-2,-1),nrow=2,ncol=1)

# No. obs:
n1 = 3
n2 = 3
n3 = 3
ni = n1+n2+n3

# Prior probabilities:
p1 = 0.25
p2 = 0.25
p3 = 0.25 

# Mean vectors:
x1_bar = matrix(c(-1,3),nrow=2,ncol=1)
x2_bar = matrix(c(1,4),nrow=2,ncol=1)
x3_bar = matrix(c(0,-2),nrow=2,ncol=1)

# Covariance matrices:
S1 = matrix(c(1,-1,-1,4),nrow=2,byrow=TRUE)
S2 = matrix(c(1,-1,-1,4), nrow = 2,byrow=TRUE)
S3 = matrix(c(1,1,1,4),nrow=2,byrow=TRUE)

# Calculate W:
W = (n1-1)*S1+(n2-1)*S2+(n3-1)*S3

# Calculate pooled covariance matrix:
g = 3
S_pooled = W/(ni-g)
Sp_inv = solve(S_pooled)

# Calculate scores:
d1 = t(x1_bar)%*%Sp_inv%*%x0-0.5*t(x1_bar)%*%Sp_inv%*%x1_bar+log(p1)
d2 = t(x2_bar)%*%Sp_inv%*%x0-0.5*t(x2_bar)%*%Sp_inv%*%x2_bar+log(p2)
d3 = t(x3_bar)%*%Sp_inv%*%x0-0.5*t(x3_bar)%*%Sp_inv%*%x3_bar+log(p3)

d1;d2;d3
max(d1,d2,d3)

# Calculate mahalanobis distance:
D1 = (-0.5)*(t(x0-x1_bar)%*%Sp_inv%*%(x0-x1_bar))+log(p1)
D2 = (-0.5)*(t(x0-x2_bar)%*%Sp_inv%*%(x0-x2_bar))+log(p2)
D3 = (-0.5)*(t(x0-x3_bar)%*%Sp_inv%*%(x0-x3_bar))+log(p3)

D1;D2;D3
max(D1,D2,D3)

#-------------------------------------------------------------------------------
# Exercise 2
#-------------------------------------------------------------------------------
