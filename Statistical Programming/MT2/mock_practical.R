# 1.1
ozone = scan("ozone.dat")
# 1.2
min(ozone)
max(ozone)
mean(ozone)
sd(ozone)
quantile(ozone)
summary(ozone)
# 1.3

boxplot(ozone)
# 1.4
# FIX: Understand QQ plots and add explanation about the normalcy
qqnorm(ozone)
qqline(ozone)
plot(ecdf(ozone), cex=0.5)
x= seq(0,4,0.01)
lines(x,pnorm(x,mean(ozone),sd(ozone)),col="red")

# 1.5
my.qqnorm = function(data.vec){
  n = length(data.vec)
  norm.quantiles = qnorm((1:n) / (n + 1))
  plot(norm.quantiles, sort(data.vec), main = "normal quantile plot",
  xlab = "Theoretical quantiles", ylab = "")
}
my.qqnorm(ozone)

# 1.6 - FIX: Why isn't there a minus one for the standard deviation too?
tobs = (mean(ozone) - 0.25) / (sd(ozone) / sqrt(length(ozone)))
pobs = 1 - pt(tobs, length(ozone)-1)

# 1.7
curve(dt(x, 11), -5, 5)
points(tobs, y=dt(tobs,length(ozone)-1), cex = 3, bg = 7, pch = 4)

# 1.8
llimit = qt(0.05, length(ozone)-1)*sd(ozone)/ sqrt(length(ozone)-1)

# 1.9
t.test(ozone,mu=0.25,alternative="greater")

# 1.10
wilcox.test(ozone,mu=0.25,alternative="greater")
# 0.17 is repeated and wilcox test assumes unique values.

# 1.11
tobs = (mean(ozone) - 0.25) / (sd(ozone) / sqrt(length(ozone)))
pobs = 1 - pt(tobs, length(ozone)-1)/2

t.test(ozone,mu=0.25,conf.level = 0.9)

# 2.1
read.table("bleeding.dat", h=T)



# 3.1

finger = read.csv("caffeine.csv", header=T)

apply(finger, 2, max)
apply(finger, 2, min)
apply(finger, 2, mean)
apply(finger,2,sd)
apply(finger, 2, median)
apply(finger, 2, function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
})


# 3.2
par(mfrow=c(3,2))
yl = "Finger Tapping"
xl = colnames(finger)

for (i in 1:3){
  boxplot(finger[i], ylim=c(235,255), xlab= xl[i], ylab = yl)
  hist(unlist(finger[i]),xlim=c(235,255), xlab= xl[i], ylab = yl)
}





