# 1.1
dt <- read.delim("C:/Users/kaany/OneDrive/Desktop/Practicals/MT3/cloud.seeding.txt")
# 1.2
head(dt)
str(dt)
# 1.3
cor(dt)[,"Y"]
# P, E, SNe, and T seem to be related to our response variables
# 1.4
plot(dt)
# There seems to be a trend between Y and T and Sne

# 1.5
library(data.table)
df = as.data.table(dt)
df$A = as.factor(df$A)
df$E = as.factor(df$E)

par(mfrow=c(2,2))
plot(df[A==1,Y])
plot(df[A==0,Y])
plot(df[E==1,Y])
plot(df[E==2,Y])
par(mfrow=c(2,2))
plot(df[A==1 & E == 1,Y])
plot(df[A==1 & E == 2,Y])
plot(df[A==0 & E == 1,Y])
plot(df[A==0 & E == 2,Y])
summary(df)



# 2.1
mT = lm(Y ~ T, data= df)

coef = coefficients(mT)
res = residuals(mT)
ft = fitted.values(mT)

# 2.2

summary(mT)

# 2.3

# Since the coefficient is negative and statistically significant at 5% confidence level
# there is evidence that the rainfalls decrease with time not increase

# 2.4
mC = lm(Y ~ C, data=df)
summary(mC)
mP = lm(Y ~ P, data=df)
summary(mP)

# The coefficients are significant at 0.001 confidence level.
# df[,C2 := C**2]
# df[,P2 := P**2]
# 2.5
mC2 = lm(Y ~ I(C^2), data=df)
summary(mC2)
mClog = lm(Y ~ log(C), data=df)
summary(mClog)
mP2 = lm(Y ~ I(P^2), data=df)
summary(mP2)
mPlog = lm(Y ~ log(P), data=df)
summary(mPlog)

# Does it make sense to compare how this model fits?
# Think about how residuals and R^2 are also scaled   
# FIX - answer

# 2.6
par(mfrow=c(2,2))
mYlog = lm(log(Y) ~ C, data=df)
summary(mYlog)
# Does it make sense to compare how this model fits?
# FIX - answer

# log Y and Y make different assumptions about the dist of Y, 
# one may be a better assumption than the other, residual plots might help
  

# 2.7
mCPT= lm(Y ~ T + C + P,data=df)
summary(mCPT)

# FIX: elaborate and add: The numbers are not the same because of confounding effects


# 2.8
mCPTA= lm(Y ~ T + C + P + A,data=df)
summary(mCPTA)
# ? Describe how A is coded as a contrast. A is converted into A1
# A is not significant. It's p-value is 68%

# 2.9
mCPTA_interact= lm(Y ~ T*A + C*A + P*A,data=df)
summary(mCPTA_interact)
# FIX: Describe

# 3.1
plot(mCPT)


# 3.2
# There are some points that are outliers such as 1,15,7 in the first figure, 
# and 1,15,2 in the second figure. So, the residuals may not be following a normal
# distribution which is one of the assumptions of the model
  

# 3.3
# FIX: Describe the concepts of leverage and influence
# Now look at the last plots, locate observations that look problematic and comment on them.

# 3.4

mCPT2 = lm(Y ~ T + C + P,data=df[-c(1,15)])
plot(mCPT2)

# FIX: Elaborate add
# The data seems to fit better