skulls = read.csv("CA1.csv")
skulls

library(dplyr)
dat = skulls %>% 
  group_by(TimePeriod) %>% 
  summarise(meanBreadth = mean(MaxBreadth),
            meanBasHeight = mean(BasHeight),
            meanLenght = mean(BasLength),
            meanNasHeight = mean(NasHeight)
  )
dat

# Question 2
library(corrplot)
library(ggplot2)
library(ggcorrplot)

T1 = skulls %>% 
  filter(TimePeriod == 1) %>% 
  select(MaxBreadth, BasHeight, BasLength, NasHeight)
R1 = cor(T1)
ggcorrplot(R1, 
           hc.order = TRUE, 
           type = 'lower',
           lab = TRUE,
           title = "Time Period 1")

T2 = skulls %>% 
  filter(TimePeriod == 2) %>% 
  select(MaxBreadth, BasHeight, BasLength, NasHeight)
R2 = cor(T2)
ggcorrplot(R2, 
           hc.order = TRUE, 
           type = 'lower',
           lab = TRUE,
           title = "Time Period 2")

T3 = skulls %>% 
  filter(TimePeriod == 3) %>% 
  select(MaxBreadth, BasHeight, BasLength, NasHeight)
R3 = cor(T3)
ggcorrplot(R3, 
           hc.order = TRUE, 
           type = 'lower',
           lab = TRUE,
           title = "Time Period 3")

T4 = skulls %>% 
  filter(TimePeriod == 4) %>% 
  select(MaxBreadth, BasHeight, BasLength, NasHeight)
R4 = cor(T4)
ggcorrplot(R4, 
           hc.order = TRUE, 
           type = 'lower',
           lab = TRUE,
           title = "Time Period 4")

T5 = skulls %>% 
  filter(TimePeriod == 5) %>% 
  select(MaxBreadth, BasHeight, BasLength, NasHeight)
R5 = cor(T5)
ggcorrplot(R5, 
           hc.order = TRUE, 
           type = 'lower',
           lab = TRUE,
           title = "Time Period 5")



# Question 3

Q3 = skulls %>% 
  filter(TimePeriod == 1) %>% 
  select(X1 = MaxBreadth, X3 = BasLength)
Q3

mB = Q3[,1]
bL = Q3[,2]
meanMB = mean(mB)
meanBL = mean(bL)
dev1 = mB - meanMB
dev2 = bL - meanBL

norm1 = sqrt(sum(dev1^2))
norm2 = sqrt(sum(dev2^2))
cor1 = (t(dev1) %*% dev2) / (norm1 * norm2)
cor1
theta = acos(cor1)
theta
#deg = theta/pi

# Bonus

bonus = skulls %>% 
  filter(TimePeriod == 1) %>% 
  select(-TimePeriod)

devs = scale(bonus, scale = F, center = T)[1:2,]
df = data.frame(
  var = colnames(devs),
  x = as.numeric(devs[1,]),
  y = as.numeric(devs[2,])
  )

ggplot(df, mapping = aes(x = x, y = y)) +
  geom_segment(aes(x = 0, y = 0, xend = x, yend = y),
               arrow = arrow()
               ) #+
  #geom_text(aes(label = var))

# Question 4
b = c(-1, 0, 0, 3)
b
Y = as.matrix(dat[,2:5]) %*% b
Y

df = data.frame(TimePeriod = dat$TimePeriod, ybar = Y)
df

mat =
  skulls %>% 
  mutate(Y = 3*NasHeight - MaxBreadth) %>% 
  group_by(TimePeriod) %>% 
  summarise(varY = var(Y))
mat

covMat = diag(mat$varY)
rownames(covMat) <- paste0("Y", mat$TimePeriod)
colnames(covMat) <- paste0("Y", mat$TimePeriod)
covMat

