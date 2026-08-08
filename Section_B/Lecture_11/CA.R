# Multivariate Section B: Correspondence Analysis
# 10 June 2026
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Example
#-------------------------------------------------------------------------------
# Specify data:
smoke = structure(c(4,4,25,18,10,
                    2,3,10,24,6,
                    3,7,12,33,7,
                    2,4,4,13,2),
                    dim=c(5L,4L),
                    dimnames=list(
                      c("SM","JM","SE","JE","SC"),
                      c("none","light","medium","heavy"))
)
smoke 

# Correspondence matrix:
P = smoke/sum(smoke)

# Row mass:
rm = apply(P,1,sum)
Dr = diag(rm)

# Column mass:
cm = apply(P,2,sum)
Dc = diag(cm)

# Standardised residuals:
S = diag(sqrt(1/rm))%*%(as.matrix(P)-rm%*%t(cm))%*%diag(sqrt(1/cm))
S

# SVD on S:
svdS = svd(S)
svdS

# Obtain principal coordinates:
PC_F = diag(sqrt(1/rm))%*%svdS$u%*%diag(svdS$d)
PC_G = diag(sqrt(1/cm))%*%svdS$v%*%diag(svdS$d)

# Obtain standard coordinates:
SC_F = diag(sqrt(1/rm))%*%svdS$u
SC_G = diag(sqrt(1/cm))%*%svdS$v

# Symmetric biplot:
plot(ca(smoke),map='symmetric',col=c('deeppink','purple'))

# Asymmetric biplot:
plot(ca(smoke),mass=TRUE,contrib='absolute',map='rowgreen',arrows=c(TRUE,TRUE))

# Summary of CA:
summary(ca(smoke))
