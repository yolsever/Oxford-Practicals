library(penalized)
library(data.table)
library(parallel)
library(randomForest)


#palette using grey
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# 1.1
setwd("C:/Users/kaany/OneDrive/Desktop/Practicals/MT5") # setting directory to read data from it
dt= fread("prepd-loblolly.txt.gz", header=T)

all(unlist(lapply(dt, is.numeric)))

# 1.2

ridge = penalized(T ~ ., data=dt,lambda2=60)

# 1.3

plot(fitted(ridge), dt[,"T"],xlab="Fitted Values", ylab="Observed Values", col= cbbPalette[3])
# plot(fitted(ridge))

abline(0,1)
text(x=-6,y=12, paste0("R^2: ", round(cor(fitted(ridge), dt$T)^2,digits= 3)))

plot_fitted <- function(fitted,y) {
  plot(fitted, y,xlab="Fitted Values", ylab="Observed Values", col= cbbPalette[3])
  abline(0,1)
  text(x=-5,y=12, paste0("R^2: ", round(cor(fitted, y)^2,digits= 3)), cex = 0.75)
}

# 1.4

plot(residuals(ridge), xlab= "Indices", ylab="Residuals", col=cbbPalette[7])
abline(0,0)

plot_res <- function(res) {
  plot(res, xlab= "Indices", ylab="Residuals", col=cbbPalette[7])
  abline(0,0) 
}


par(mfrow = c(1, 2))
plot_fitted(fitted(ridge), dt$T)
plot_res(residuals(ridge))




# 1.5
one_fold_cv <- function(model="ridge",dt, fold, l2) {
  model_ = penalized(T ~ ., data=dt[grp !=fold,-c("grp")],lambda2=l2,trace=F)
  preds = predict(model_, dt[grp==fold,-c("T", "grp")])[,"mu"]
  return(list(dt[grp==fold,T], preds))
}

cv <- function(dt,nfold=10, l2=60) {
  n <- 1:nfold; nn <- nrow(dt)
  g <- factor(sample(1:nfold, nn, replace=T))
  dt[,grp := g]
  lapply(1:10,function(x) {
    one_fold_cv(dt,x, l2)
  })
}

res <- cv(dt)

# 1.6
obs = unlist(lapply(res, `[[`, 1))
fitt= unlist(lapply(res, `[[`, 2))

plot_fitted(fitt,obs)
plot_res(obs-fitt)


# 1.7

par_cv <- function(dt, cores=8,nfold=10, l2=60) {
  n <- 1:nfold; nn <- nrow(dt)
  g <- factor(sample(1:nfold, nn, replace=T))
  dt[,grp := g]
  cl <- makeCluster(cores)
  clusterExport(cl=cl, "one_fold_cv")
  clusterEvalQ(cl=cl, library("penalized"))
  clusterEvalQ(cl=cl, library("data.table"))
  res=parLapply(cl=cl, 1:10, one_fold_cv, dt=dt,l2=60)
  stopCluster(cl)
  return(res)
}

par_cv(dt)

# 1.8
time_1 = system.time(par_cv(dt, cores=1))
time_2 = system.time(par_cv(dt, cores=2))
time_4 = system.time(par_cv(dt, cores=4))


# 1.9

one_fold_cv <- function(model,dt, fold, l2,ntree) {
  if (model =="ridge") {
    model_ = penalized(T ~ ., data=dt[grp !=fold,-c("grp")],lambda2=l2,trace=F)
  }
  else if (model =="randomForest") {
    model_ = randomForest(T ~ ., data=dt[grp !=fold,-c("grp")], ntree = ntree)
  }
  preds = predict(model_, dt[grp==fold,-c("T", "grp")])[,"mu"]
  return(list(dt[grp==fold,T], preds))
}

par_cv <- function(dt, model="ridge",cores=8,nfold=10, l2=60,ntree=20) {
  n <- 1:nfold; nn <- nrow(dt)
  g <- factor(sample(1:nfold, nn, replace=T))
  dt[,grp := g]
  cl <- makeCluster(cores)
  clusterExport(cl=cl, "one_fold_cv")
  clusterEvalQ(cl=cl, library("penalized"))
  clusterEvalQ(cl=cl, library("randomForest"))
  clusterEvalQ(cl=cl, library("data.table"))
  res=parLapply(cl=cl, 1:10, one_fold_cv, dt=dt,l2, ntree,model="ridge")
  stopCluster(cl)
  return(res)
}

# 1.10
ridge = par_cv(dt)
randomforest = par_cv(dt, model = "randomForest")

obs = unlist(lapply(ridge, `[[`, 1))
fitt= unlist(lapply(ridge, `[[`, 2))

par(mfrow=c(1,2))
plot_fitted(fitt,obs)
plot_res(obs-fitt)


obs = unlist(lapply(randomforest, `[[`, 1))
fitt= unlist(lapply(randomforest, `[[`, 2))

plot_fitted(fitt,obs)
plot_res(obs-fitt)
