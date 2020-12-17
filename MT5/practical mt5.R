library(penalized)
library(data.table)


#palette using grey
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# 1.1
setwd("C:/Users/kaany/OneDrive/Desktop/Practicals/MT5") # setting directory to read data from it
dt = read.table(gzfile("prepd-loblolly.txt.gz"), header=T)

all(unlist(lapply(dt, is.numeric)))

# 1.2

ridge = penalized(T ~ ., data=dt,lambda2=60)

# 1.3

plot(fitted(ridge), dt[,"T"],xlab="Fitted Values", ylab="Observed Values", col= cbbPalette)
# plot(fitted(ridge))

abline(0,1)
text(x=-6,y=12, paste0("R^2: ", round(cor(fitted(ridge), dt$T)^2,digits= 3)))

# 1.4

plot(residuals(ridge))
abline(0,0)

par(mfrow = c(1, 2))


# 1.5


