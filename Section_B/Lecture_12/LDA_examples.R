# Multivariate Analysis Section B: Fishers Linear Discriminant Analysis
# 7 June 2026
# Isabella Lethbridge 

#-------------------------------------------------------------------------------
# Two Group Example
#-------------------------------------------------------------------------------
# Setup:
set.seed(2025)
library(ggplot2)
library(cowplot)
library(dplyr)

# Simulate data for 2 groups (p=2):
n     = 20
Sigma = matrix(c(3,0,0,1),2,2)
means = list(c(-0.5,-3),c(0.5,3))
dat   = do.call(rbind,lapply(1:2, function(i){
  data.frame(group=factor(i),
             MASS::mvrnorm(n,mu=means[[i]],Sigma=Sigma))
}))
colnames(dat)[-1]=c("X1","X2")
ggplot(dat,
       aes(x=X1,y=X2,color=group)) +
  geom_point(size=3) +
  theme_cowplot() +
  background_grid(major="xy") +
  labs(title="Simulated Data for LDA",x="X1",y="X2")

# Obtain matric W^-1B:
# Extract predictors 
X = as.matrix(dat[,2:3])

# No. different responses:
g = length(unique(dat$group))

# Store no. obs from each group in table:
n_i = table(dat$group)

# Calculate overall mean:
bar_overall = colMeans(X)

# Calculate group means:
bar_group = dat|>
  group_by(group)|>
  summarise(
    across(starts_with("X"),mean),
    .groups='drop'
  )|>
  select(-group)|>
  as.matrix()

# No. predictors 
p = ncol(X)
B = matrix(NA,nrow=p,ncol=p)
W = matrix(NA,nrow=p,ncol=p)

# For loop to calculate B:
for (i in 1:g)
{
  dev = as.matrix(bar_group[i,]-bar_overall)
  print(dev)
  B = B+n_i[i]*(dev%*%t(dev))
}

# For loop to calculate W:
for (i in 1:g)
{
  Xi = X[dat$group==i,] # extract data belonging to group i
  W  = W+cov(Xi)*(ni_[i]-1)
}

# Miguels method: 
B = Reduce(`+`,lapply(1:g,function(i){
  n_i[i]*(bar_group[i,]-bar_overall)%*%t(bar_group[i,]-bar_overall)
}))

W <- Reduce(`+`, lapply(1:g, function(i){
  Xi <- X[dat$group==i,]
  cov(Xi) * (n_i[i]-1)
}))

# Eigen decomposition:
inv_WB  = solve(W)%*%B
eig_obj = eigen(inv_WB)
eig_vec = eig_obj$vectors # this is a in notes

# Calculate pooled variance:
S_pooled = W/(sum(n_i)-g)

# Calculate:
sd_vec = diag(t(eig_vec)%*%S_pooled%*%eig_vec)|>sqrt()

eig_vec = eig_vec%*%diag(1/sd_vec)

# Plot first new variable:
dat$LD1 = as.numeric(as.matrix(X)%*%eig_vec[,1])

# Plot LD1 histogram
ggplot(dat,
       aes(x=LD1,fill=group))+
  geom_histogram(alpha=0.6,position="identity",bins=20)+
  theme_cowplot()+
  labs(title="LDA Projection (First Discriminant)",x="LD1",y="Count")

# The first PC weighs heavily on second variable:
eig_obj = eigen(cov(X))
eig_obj$vectors[,1:2]|> 
  signif(2)|> 
  knitr::kable(
    col.names=c("PC1", "PC2"),
    caption="Eigenvectors of $X'X$ (Principal Components)"
  )

# Plot of first two linear discriminants is essentially the same as the plot of the raw data but with axes flipped:
dat$LD2 = as.numeric(as.matrix(X)%*%eig_vec[,2])
ggplot(dat,aes(x=LD1,y=LD2,color=group)) +
  geom_point()+
  theme_cowplot()+
  background_grid(major = "xy")+
  labs(title="LDA Projection (First Two Discriminants)",x="LD1",y="LD2")

# Loading vectors are same as those from MASS:lda:
MASS::lda(group ~ .,data=dat[,1:3])$scaling|>
  signif(2)|>
  knitr::kable(
    col.names=c("LD1", "LD2"),
    caption="Loading Vectors from `MASS::lda`"
  )

#-------------------------------------------------------------------------------
# Multiple-group Example
#-------------------------------------------------------------------------------
# Setup:
set.seed(2025)
library(ggplot2)
library(cowplot)
library(dplyr)
library(knitr)

# Simulate five groups, p = 10
n = 20       # observations per group
p = 10       # number of variables

# Common covariance
# Diagonal matrix with variance 3 on X1, and 1 on X2–X10
Sigma = diag(c(3,rep(1,p-1)))

# Define group means so that they differ along the first five dimensions:
means = lapply(1:5,function(i){
  mu = rep(0,p)
  mu[i] = 3*(i-3)             # shifts:−6,−3,0,3,6 on dimension i
  mu
})

# Simulate the data:
dat = do.call(rbind,lapply(1:5,function(i) {
  data.frame(
    group = factor(i),
    MASS::mvrnorm(n,mu=means[[i]],Sigma=Sigma)
  )
}))

# Name the columns X1…X10:
colnames(dat)[-1] = paste0("X",seq_len(p))

head(dat)

# Step 1: Dimensions 
X   = as.matrix(dat[,-1])        # predictor variables 
g   = length(unique(dat$group))  # no. groups 
n_i = table(dat$group)           # no. obs per group

bar_overall = colMeans(X)        # predictor means 
bar_group   = dat|>              # group means 
  group_by(group)|>
  summarise(
    across(everything(),mean),
    .groups='drop'
  )|>
  select(-group)|>
  as.matrix()

# Step 2: Calculate B and W
# Calculate B:
B = Reduce(`+`,lapply(1:g,function(i){
  n_i[i]*(bar_group[i,]-bar_overall)%*%t(bar_group[i,]-bar_overall)
}))

# Calculate W:
W = Reduce(`+`,lapply(1:g,function(i){
  Xi=X[dat$group==i,]
  cov(Xi)*(n_i[i]-1)
}))

# Step 3: Eigen decomposition
inv_WB   = solve(W)%*%B
eig_WB   = eigen(inv_WB)
eig_vec  = eig_WB$vectors|>
  Re()
S_pooled = W/(sum(n_i)-g)
sd_vec   = diag(t(eig_vec)%*%S_pooled%*%eig_vec)
sd_vec   = sqrt(sd_vec)
eig_vec  = eig_vec%*%diag(1/sd_vec)
eig_vec  = eig_vec[,1:2]

display_tbl = data.frame(
  "Variable" = paste0("V",seq_len(nrow(eig_vec))),
                "LD1"=eig_vec[,1]|>
                  signif(2),
                "LD2"=eig_vec[,2]|>
                  signif(2)
)

kable(
  display_tbl,
 col.names = c("Variable","LD1","LD2"),
 caption = "Scaled eigenvectors pf $W^{-1}B$ (loading vectors)."
)

# Step 4: Calculate scores
scores = X%*%eig_vec

# Add scores to datadset:
dat$LD1 = as.numeric(scores[,1])
dat$LD2 = as.numeric(scores[,2])

# Step 5: Plot first two scores to visualise class separation
# Pretty colours:
col_vec <- c(
  "#1b9e77",
  "#d95f02",
  "#7570b3",
  "#e7298a",
  "#66a61e"
) |>
  toupper()

# Plot scores:
ggplot(dat,
       mapping=aes(x=LD1,y=LD2,colour=group))+
  geom_point(size=3,alpha=0.8)+
  theme_minimal()+
  labs(
    title="LDA projection of first two discriminants",
    x = "LD1",
    y = "LD2"
  )+
  scale_colour_manual(
    values=col_vec,
    name="Group"
  )
# Step 6: Interpret 
# Check with built in library:
MASS::lda(group~.,data=dat[,1:11])$scaling[,1:2]|>
  signif(2)
