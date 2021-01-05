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

setwd(file.path("C:/Users/kaany/OneDrive/Desktop/Practicals/MT7"))

bw <- read.csv("bw.csv")

# brief hints
bw$race <- as.factor(bw$race)
bw$ptl <- as.factor(bw$ptl)
# to be able to refer to a column as e.g. race rather than bw$race
# here it is convenient to:
attach(bw)
# use detach(bw) to remove it when finished, can check using search()
# For plot examples
(tab1 <- table(low, race))
barplot(tab1, beside = TRUE)
# can use e.g. names.arg and col arguments of barplot() to improve plot
boxplot(mwt ~ low, xlab='low bw', ylab='mother weight')

str(bw)

glm1 = glm(low ~ ., data= bw, family=binomial)

# I choose binomial as the response variable is binary.
# My priors are negative: age, race (black, other), smoke, ht  
#               positive: mwt, ptl, 