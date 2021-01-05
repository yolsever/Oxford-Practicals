library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(leaps)
library(MuMIn)
library(MASS)
library(grid)
library(stargazer)
library(xtable)

options(na.action = "na.fail")

swim <- read.csv("C:/Users/kaany/OneDrive/Desktop/Practicals/MT4/swim.csv", stringsAsFactors=T)

swim = as.data.table(swim)
swim[,dist_factor:= as.factor(dist)]
# 1.1
str(swim)
head(swim)
tail(swim)
par(mfrow= c(1,2))


summary(swim[,-c("time","event")])

pdf("Pairs Plots for The Variables.pdf")
pairs(swim, lower.panel = NULL, main="Pairs Plots for The Variables")
dev.off()

pdf("event_bar_chart.pdf")
ggplot(swim, aes(x=event)) +
  geom_bar(fill = "navy") +
  ggtitle("Bar Chart for The Distribution of Events") +
  labs(y="Count", x = "Type of the Event") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        plot.title = element_text(hjust = 0.5))
dev.off()

pdf("dist of time.pdf")
par(mfrow= c(1,2))
boxplot(swim$time, ylim=c(min(swim$time),max(swim$time)), ylab="Time", 
        main = "Box and Whiskers Plot for Time")
plot(swim$time, xlab="Observation", ylab="Time", 
     main="Scatter Plot for Time") <- 
dev.off()

pdf("Time by Categories.pdf")
g1 = qplot(y=time, data = swim, colour = swim$dist)
g2 = qplot(y=time, data = swim, colour = swim$sex)
g3 = qplot(y=time, data = swim, colour = swim$stroke)
g4 = qplot(y=time, data = swim, colour = swim$course)
my_plots <- list(g1, g2, g3,g4)
my_layout <- rbind(c(1, 3), c(2, 4))
grid.arrange(grobs = my_plots, layout_matrix = my_layout, 
             top = textGrob("Time by Categories"))
dev.off()

# swim[,dist:= as.integer(dist)]
# 1.2
# lm1 <- lm(log(time) ~ (dist_factor + stroke + sex + course)^2, data = swim)
lm1 <- lm(log(time) ~ (course +log(dist) +  sex + stroke)^2, data = swim)

b = dredge(lm1)
best_lm = get.models(b, 1)[[1]]
# qqnorm(rstudent(best_lm), main = "Q-Q plot of \n studentised residuals")
# qqline(rstudent(best_lm))

s <- stargazer(lm1, best_lm, title="Regression Results", align=TRUE,column.sep.width = "0.4pt",
          font.size="tiny")
fileConn<-file("stargazer.txt")
writeLines(s, fileConn)
close(fileConn)

  # best_lm = lm(log(time) ~ course + log(dist) + sex + stroke + 
#      course:sex + course:stroke + log(dist):sex + log(dist):stroke + 
#      sex:stroke + 1, data = swim)

summary(best_lm)

par(mfrow=c(1,1))
pdf("Box Cox for the best model.pdf")
boxcox(lm1, lambda=seq(-2,2,1/10))
dev.off()
res = boxcox(lm1, lambda=seq(-2,2,1/10), plotit=F)
dev.off()
lambda = res$x[which(res$y==max(res$y))]



pdf("residuals vs fitted values.pdf")
par(mfrow=c(1,2))
plot(resid(best_lm) ~ fitted(best_lm), data = swim,
     xlab = "Fitted values", ylab = "Residuals")
plot(rstudent(best_lm) ~ fitted(best_lm), xlab = "Fitted values",
     ylab = "(Studentised) residuals")
dev.off()

pdf("qq plots for the residuals.pdf")
par(mfrow = c(1,2))
plot(rstudent(best_lm) ~ fitted(best_lm), main = "Studentised residuals \n vs. \n Fitted Values",
     xlab = "Fitted values", ylab = "(Studentised) residuals")
qqnorm(rstudent(best_lm), main = "Q-Q plot of \n studentised residuals")
qqline(rstudent(best_lm))
dev.off()


(n <- dim(swim)[1])
# (p <- dim(swim)[2])
# Hard-coding this to account for the interaction terms in the model 
p <- length(coef(best_lm))

(i <- cooks.distance(best_lm) > (8/(n - 2*p)))

pdf("Pairs plot with red outliers.pdf")
pairs(swim, lower.panel = NULL, pch = 1 + 15*i, col = 1 + i)
dev.off()

pdf("Model diagnosis with red dots.pdf")
par(mfrow = c(2, 2))
qqnorm(rstudent(best_lm), main = NULL, pch = 1 + 15*i, col = 1 + i)
qqline(rstudent(best_lm))

plot(fitted.values(best_lm), rstudent(best_lm), pch = 1 + 15*i, col = 1 + i)
# text(fitted.values(best_lm), rstudent(best_lm), abbreviate(row.names(sw)), adj = -0.2)

# Leverage values
plot(hatvalues(best_lm), ylim = c(min(hatvalues(best_lm))*1/2, max(hatvalues(best_lm))*5/3), pch = 1 + 15*i, col = 1 + i)
# text(hatvalues(best_lm), row.names(sw), srt = 90, adj = -0.1)
abline(2*p/n, 0, lty = 2)

plot(cooks.distance(best_lm), ylim = c(min(cooks.distance(best_lm)) * 1/2, max(cooks.distance(best_lm)) * 5/3), pch = 1 + 15*i, col = 1 + i)
# text(cooks.distance(best_lm), row.names(swim), srt = 90, adj = 1.1)
abline(8/(n - 2*p), 0, lty = 2)
dev.off()

# matrix(c(1,2,3,4, 1,2,3,4), nrow=2)

data <- data.table(dist = c(400,50,400,100), 
                   stroke= c("Freestyle","Backstroke", "Butterfly","Medley"), 
                   sex= rep("F",4), 
                   course= rep("Long", 4))
data[,dist_factor:=as.factor(dist)]

# data[,event:= paste0(dist, " m ", stroke)]
# data[,time:=NA]
# data[,time_bc := time^lambda]
pred_table = cbind(data,exp(predict(best_lm,as.data.frame(data), interval="predict")))

xtable(pred_table)
# Sanity check
mean(swim[dist==400 & stroke=="Freestyle" & sex=="F" & course=="Long",time])
(mean(swim[dist== 100 & stroke == "Backstroke" & sex== "F" & course=="Long", time])/2 + mean(swim[dist== 50 & stroke == "Freestyle" & sex== "F" & course=="Long", time]))/2
(mean(swim[dist== 200 & stroke == "Butterfly" & sex== "F" & course=="Long", time])*2 + mean(swim[dist== 400 & stroke == "Medley" & sex== "F" & course=="Long", time]))/2
(mean(swim[dist== 200 & stroke == "Medley" & sex== "F" & course=="Long", time])/2+ mean(swim[dist== 100 & stroke == "Butterfly" & sex== "F" & course=="Long", time]))/2






