############################################################
# PACKAGES
############################################################

library(dplyr)
library(ca)
library(MASS)

############################################################
# QUESTION 2.1
# CORRESPONDENCE ANALYSIS
############################################################

dat$Fertilizer <- factor(dat$Fertilizer)
dat$Irrigation <- factor(dat$Irrigation)

tab <- table(dat$Fertilizer, dat$Irrigation)

tab

ca_fit <- ca(tab)

summary(ca_fit)

plot(
  ca_fit,
  map = "rowprincipal",
  main = "Row-principal CA: Fertilizer vs Irrigation"
)

############################################################
# QUESTION 2.2(a)
# MULTIVARIATE REGRESSION FROM FIRST PRINCIPLES
############################################################

Y <- as.matrix(
  dat[, c("CropYield", "WindSpeed")]
)

X <- model.matrix(
  ~ Rainfall + TempC,
  data = dat
)

Bhat <- solve(
  t(X) %*% X,
  t(X) %*% Y
)

rownames(Bhat) <- c(
  "(Intercept)",
  "Rainfall",
  "TempC"
)

colnames(Bhat) <- c(
  "CropYield",
  "WindSpeed"
)

Bhat

############################################################
# QUESTION 2.2(b)
# MANOVA TEST FOR RAINFALL
############################################################

X_full <- model.matrix(
  ~ Rainfall + TempC,
  data = dat
)

B_full <- solve(
  t(X_full) %*% X_full,
  t(X_full) %*% Y
)

E_full <- t(Y - X_full %*% B_full) %*%
  (Y - X_full %*% B_full)

X_red <- model.matrix(
  ~ TempC,
  data = dat
)

B_red <- solve(
  t(X_red) %*% X_red,
  t(X_red) %*% Y
)

E_red <- t(Y - X_red %*% B_red) %*%
  (Y - X_red %*% B_red)

H <- E_red - E_full

wilks_lambda <- det(E_full) / det(E_red)

wilks_lambda

fit_manova <- manova(
  cbind(CropYield, WindSpeed) ~ Rainfall + TempC,
  data = dat
)

summary(
  fit_manova,
  test = "Wilks"
)

############################################################
# QUESTION 2.2(c)
# PCR EXPLORATION
############################################################

X_pcr <- dat %>%
  select(
    -FarmID,
    -CropYield,
    -TempF
  )

X_pcr <- model.matrix(
  ~ .,
  data = X_pcr
)[,-1]

pca_fit <- prcomp(
  X_pcr,
  center = TRUE,
  scale. = TRUE
)

summary(pca_fit)

plot(
  pca_fit,
  type = "l",
  main = "PCR Scree Plot"
)

pc_scores <- pca_fit$x

cor(
  pc_scores[,1:5],
  dat$CropYield
)

############################################################
# QUESTION 2.2(d)
# FIRST PLS COMPONENT FROM FIRST PRINCIPLES
############################################################

X_pls <- dat %>%
  select(
    -FarmID,
    -CropYield,
    -TempF
  )

X_pls <- model.matrix(
  ~ .,
  data = X_pls
)[,-1]

X_pls <- scale(X_pls)

y_pls <- scale(dat$CropYield)

w <- crossprod(
  X_pls,
  y_pls
)

w <- as.vector(w)

w <- w / sqrt(sum(w^2))

t1 <- X_pls %*% w

cor(
  as.vector(t1),
  dat$CropYield
)

############################################################
# QUESTION 2.3
# CANONICAL CORRELATION ANALYSIS
############################################################

X1 <- dat %>%
  select(
    Rainfall,
    TempC,
    Sunlight,
    SoilPH,
    WindSpeed,
    DistanceToWater,
    Altitude
  )

X2 <- model.matrix(
  ~ Fertilizer +
    PesticideApplied +
    Irrigation,
  data = dat
)[,-1]

X1_std <- scale(X1)
X2_std <- scale(X2)

cca_fit <- cancor(
  X1_std,
  X2_std
)

############################################################
# QUESTION 2.3(a)
############################################################

cca_fit$cor[1]

cca_fit$xcoef[,1]

cca_fit$ycoef[,1]

############################################################
# QUESTION 2.3(b)
############################################################

U2 <- X1_std %*%
  cca_fit$xcoef[,2]

V2 <- X2_std %*%
  cca_fit$ycoef[,2]

head(U2)

head(V2)

cor(U2, V2)

cca_fit$cor[2]

############################################################
# QUESTION 2.3(c)
############################################################

reg_cca <- lm(
  V2 ~ .,
  data = as.data.frame(X1_std)
)

fitted_vals <- fitted(reg_cca)

cor(
  fitted_vals,
  V2
)

cca_fit$cor[2]

summary(reg_cca)

############################################################
# QUESTION 2.4
# LINEAR DISCRIMINANT ANALYSIS
############################################################

lda_fit <- lda(
  Fertilizer ~
    Rainfall +
    TempC +
    Sunlight +
    SoilPH +
    WindSpeed +
    DistanceToWater +
    Altitude,
  data = dat
)

lda_fit

############################################################
# QUESTION 2.4(a)
# DISCRIMINANT SCORES PLOT
############################################################

lda_scores <- predict(
  lda_fit
)$x

plot(
  lda_scores[,1],
  lda_scores[,2],
  col = as.numeric(dat$Fertilizer),
  pch = 19,
  xlab = "LD1",
  ylab = "LD2",
  main = "LDA: Fertilizer Groups"
)

legend(
  "topright",
  legend = levels(dat$Fertilizer),
  col = 1:length(levels(dat$Fertilizer)),
  pch = 19
)

############################################################
# QUESTION 2.4(b)
# LOADINGS
############################################################

lda_fit$scaling

abs(lda_fit$scaling)

loadings_table <- as.data.frame(
  lda_fit$scaling
)

loadings_table