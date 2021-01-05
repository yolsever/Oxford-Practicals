library(data.table)
library(dplyr)
library(stargazer)
library(xtable)
library(rsq)
require(plotrix)
library('plyr')

setwd(file.path("C:/Users/kaany/OneDrive/Desktop/Practicals/MT7"))

dt = fread("dvis.csv")

pdf("educyrs vs eductype.pdf")
plot(unique(dt[,list(eductype, educyrs)]))
dev.off()

# Clean eductype, educyrs
dt = dt[educyrs %% 0.5 == 0,]

# Drop educyrs. Leave eductype because more informative. More school doesnt mean 
# better quality or more education. It could be repetition etc
# TODO: Talk about why this makes sense, maybe add a test or sth
cor(dt)

dt[,educyrs := NULL]
dt[,hhninc:=log(hhninc*1000)]

# Convert variables to factor
dt[,female:=as.factor(female)]
dt[,hhkids:=as.factor(hhkids)]
dt[,married:=as.factor(married)]
dt[,employed:=as.factor(employed)]
dt[,addins:=as.factor(addins)]
dt[,privateins:=as.factor(privateins)]
dt[,eductype:=as.factor(eductype)]

summary(dt[,-c("age","hhninc","docvis")])

dt_ = dt[female==0,]
dt_[,female := NULL]

lm(docvis ~ ., data=dt_)

# 4.1

# Get frequencies
pdf("Binary Variables freq.pdf")
par(mfrow = c(2,3))
barplot(table(dt$female), main = "Sex", names.arg=c("Male","Female"), ylab="Frequency", ylim=c(0,800))
barplot(table(dt$hhkids), main = "Child under 16", names.arg = c("No", "Yes"), ylab="Frequency",ylim=c(0,800))
barplot(table(dt$married), main ="Married", names.arg=c("Other", "Married"), ylab="Frequency",ylim=c(0,1000))
barplot(table(dt$employed), main = "Employed", names.arg=c("No", "Yes"), ylab="Frequency",ylim=c(0,1000))
barplot(table(dt$addins), main = "Addt. Insurance", names.arg=c("No", "Yes"), ylab="Frequency",ylim=c(0,1200))
barplot(table(dt$privateins), main= "Private Insurance", names.arg = c("No", "Yes"), ylab="Frequency",ylim=c(0,1200))
dev.off()

pdf("Non-binary variables.pdf")
par(mfrow = c(2,2))
barplot(table(dt$eductype), main= "Highest Degree", ylab="Frequency",ylim=c(0,800))
hist(exp(dt$hhninc), main="Monthly Income", ylab="Frequency", xlab="")
barplot(count(dt$age)$freq ~ count(dt$age)$x, main="Age", ylab="Frequency", xlab="",ylim=c(0,50))
barplot(table(dt$docvis), main="Doctor Visits", ylab="Frequency",ylim=c(0,600))
dev.off()

dt[,agegroup:=20]
dt[age < 40 & age >= 30, agegroup:=30]
dt[age < 50 & age >= 40, agegroup:=40]
dt[age < 60 & age >= 50, agegroup:=50]
dt[age < 70 & age >= 60, agegroup:=60]

pdf("Docvis against age and Income.pdf")
par(mfrow = c(1, 2))
m = aggregate(dt$docvis, list(dt$agegroup), FUN= "mean")$x
s = aggregate(dt$docvis, list(dt$agegroup), FUN= "sd")$x

plotCI(sort(as.integer(unique(dt$agegroup))),m, ui=m+s/2, li=m-s/2, xlab= "Age", ylab= "Doctor Visits",
       main="Doctor Visits against Age Groups")
dt[,agegroup:=NULL]

m = aggregate(dt$hhninc, list(dt$docvis), FUN= "mean")$x
s = aggregate(dt$hhninc, list(dt$docvis), FUN= "sd")$x

plotCI(sort(unique(dt$docvis)),m, ui=m+s/2, li=m-s/2, xlab= "Doctor Visits",
       ylab= "Log Monthly Household Income",
       main="Doctor Visits against Log Income")
dev.off()

pdf("Doctor Visits against Categorical Variables.pdf")
par(mfrow=c(1,4))
boxplot(docvis ~ eductype, data=dt)
boxplot(docvis ~ female, data = dt)
boxplot(docvis ~ hhkids, data = dt)
boxplot(docvis ~ addins, data = dt)
dev.off()
pdf("Doctor Visits against Categorical Variables_2.pdf")
par(mfrow=c(1,3))
boxplot(docvis ~ employed, data =dt)
boxplot(docvis ~ privateins, data =dt)
boxplot(docvis ~ married, data =dt)
mtext("Doctor Visits against Categorical Variables", side = 3, line = -3, outer = TRUE)
dev.off()

# Interaction Terms

# par(mfrow= c(1,2))
# boxplot(docvis ~ eductype * privateins, data= dt, las = 2, col =c("orange", "yellow"))
# boxplot(docvis ~ eductype * female, data= dt, las = 2, col=c("orange", "yellow"))

# 4.2 Poisson GLM & Model Selection
glm1 <- glm(docvis ~ (.)^2, data= dt, family=poisson)

summary(glm1)
rsq(glm1)

bl = glm(docvis ~ (.)^2, data= dt, family=poisson)
best = 100000
# Iterate over to ensure we get the best model because AIC depends on order
# Scope undefined defaults to backwards
for  (i in range(1,8)) {
  temp = step(glm(docvis ~ (.)^2, data= sample(dt), family=poisson),direction="both") 
  print(temp$aic)
  if (temp$aic < best) {
    bl = temp
    best = temp$aic
  }
}

summary(bl)
rsq(bl)
anova(bl, test="Chisq")


1 - pchisq(bl$deviance - glm1$deviance, length(coef(glm1)) - length(coef(bl)))

pdf("Deviance Residuals.pdf")
par(mfrow=c(1,1))
plot(predict(bl,type="response"), rstandard(bl),
     xlab=expression(hat(mu)), ylab="Standardized Deviance Residuals",
     pch=dt$docvis)
legend(4,3.5, sort(unique(dt$docvis)), cex=0.85, pch=sort(unique(dt$docvis)), title="Doctor Visits")
dev.off()

s <- stargazer(bl, title="Regression Results", align=TRUE,column.sep.width = "0.4pt",
               font.size="tiny")
fileConn<-file("stargazer.txt")
writeLines(s, fileConn)
close(fileConn)

# Check if we can drop any variable
for (i in 1:length(dt)){
  if (i == which(colnames(dt) == "docvis")) {
    next
  }
  glm2 = glm(docvis ~ (.)^2, data=subset(dt, select= -c(i)))
  print(1 - pchisq(glm2$deviance - glm1$deviance, length(coef(glm1)) - length(coef(glm2))))
}

# married-eductype
test2 <- glm(formula = docvis ~ employed + hhkids + addins + hhninc + 
               married + age + privateins + female + eductype + employed:hhkids + 
               employed:addins + employed:married + employed:age + hhkids:hhninc + 
               addins:hhninc + addins:age + hhninc:married +
               age:female + privateins:eductype, family = poisson, data = sample(dt))

1 - pchisq(test2$deviance - bl$deviance,  length(coef(bl)) - length(coef(test2)))
rsq(test2)

# privateins-eductype
test3 <- glm(docvis ~ employed + hhkids + addins + hhninc + 
               married + age + privateins + female + eductype + employed:hhkids + 
               employed:addins + employed:married + employed:age + hhkids:hhninc + 
               addins:hhninc + addins:age + hhninc:married + married:eductype + 
               age:female, family = poisson, data = sample(dt))

1 - pchisq(test3$deviance - bl$deviance,  length(coef(bl)) - length(coef(test3)))

rsq(test3)

# hhkids1:hhninc
test4 <- glm(docvis ~ employed + hhkids + addins + hhninc + 
               married + age + privateins + female + eductype + employed:hhkids + 
               employed:addins + employed:married + employed:age + 
               addins:hhninc + addins:age + hhninc:married + married:eductype + 
               age:female, family = poisson, data = sample(dt))

1 - pchisq(test4$deviance - bl$deviance,  length(coef(bl)) - length(coef(test4)))
rsq(test4)

# age:employed
test5 <- glm(docvis ~ employed + hhkids + addins + hhninc + 
               married + age + privateins + female + eductype + employed:hhkids + 
               employed:addins + employed:married + hhkids:hhninc + 
               addins:hhninc + addins:age + hhninc:married + married:eductype + 
               age:female, family = poisson, data = sample(dt))

1 - pchisq(test5$deviance - bl$deviance,  length(coef(bl)) - length(coef(test5)))

s <- stargazer(bl, test3, title="Regression Results", align=TRUE,column.sep.width = "0.4pt",
               font.size="tiny")
fileConn<-file("stargazer.txt")
writeLines(s, fileConn)
close(fileConn)

bl = test3

# 4.3 Model Fit

p <- length(coef(bl) - 1)
n <- nrow(dt)

pdf("leverage and influence.pdf")
par(mfrow=c(1,2))
plot(hatvalues(bl),
     pch=19, col='blue', ylab='Leverage / (p/n)')

abline(2*p/n, 0,lty=2, col="red")

plot(cooks.distance(bl),
     pch=19, col='blue', ylab="Cook's Distance")

abline(8/(n - 2*p), 0, lty = 2, col = "red")
dev.off()

dt[which(hatvalues(bl) > 0.3,)]
dt[which(cooks.distance(bl) > 0.1),]

dt[which(hatvalues(bl) > 2*p/n),]
dt[which(cooks.distance(bl) > 8/(n-2*p)),]

qqnorm(rstandard(glm1), pch=19, main="")
qqline(rstandard(glm1))

lambda <- bl$null.deviance - bl$deviance

1 - pchisq(lambda, p)

rsq.kl(bl)

xtable(anova(bl, test="Chisq"))
# 4.4 Interpretation


# 4.5 Estimating Dispersion Parameter
mu = predict(test3, type="response")
phi = 1/(n-p)*sum((dt$docvis- mu)^2/mu)

s <- stargazer(test3, title="Regression Results (Dispersion Adjusted)", align=TRUE,column.sep.width = "0.4pt",
               font.size="tiny", apply.se = function(x) {x*phi^0.5})
fileConn<-file("regression with dispersion fixed.txt")
writeLines(s, fileConn)
close(fileConn)


