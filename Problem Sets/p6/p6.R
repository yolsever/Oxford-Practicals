library(parallel)

# 1a
cl <- makeCluster(detectCores()-1)
socket_results <- parSapply(
  cl = cl, X = 1:100,n=100, FUN = function(x,n) {
    mean(rnorm(n))
  }
)
stopCluster(cl)


# 1b
cl <- makeCluster(detectCores()-1)
socket_results <- parSapply(cl=cl,
                            X=mtcars,FUN=function(x) {
                              sum(x)/length(x)
                            }
)
stopCluster(cl)

# 2a
library(data.table)
dt = read.csv("C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/p6/autompg_clean.csv",row.names=1)
train= dt[sample(1:nrow(dt),ceiling(nrow(dt)*0.8)),]
test= dt[-sample(1:nrow(dt),ceiling(nrow(dt)*0.8)),]

bootstrap_lm <- function(pct, repl=TRUE){
  lm(train[sample(1:nrow(train),size=ceiling(nrow(train)*pct),replace=repl),]$mpg~as.matrix(train[sample(1:nrow(train),size=ceiling(nrow(train)*pct),replace=repl),][,c(-1,-length(train))]))
}

cl <- makeCluster(detectCores()-1)
clusterExport(cl=cl,c("train","bootstrap_lm"))

socket_results <- parSapply(cl=cl,
                            X=1:100,pct=0.3,FUN=function(x,pct) {
                              bootstrap_lm(pct)
                            }
)
stopCluster(cl)

# 1d - FIX: get the estimate

# 3a

sample_with_repl <- function(x,n){
  x[ceiling(runif(length(x),0,length(x)))]
}

# 3b
"Computational complexity of this function is O(n)"

# 3c
library(microbenchmark)

res = sapply(2**seq(1,15), function(x) {
  microbenchmark(sample_with_repl(sample_with_repl(runif(x),x)),times=100)
})
mean_res = sapply(1:15, function(x) {
  mean(unlist(res[2,x]))
})

plot(2**seq(1,15), mean_res)
lines(2**seq(1,15), mean_res)
# lines(2**seq(10,15), 2*2**seq(10,15))

# 3d
"Computational complexity is O(B(M+N))"

# 3e
bootstrap_hypo_test <- function(x,y,B=10) {
  t = mean(x) - mean(y) / (sd(x)**2/length(x)+sd(y)**2/length(y))
  
  x_new = x- mean(x) + mean(c(x,y))
  y_new = y - mean(y) + mean(c(x,y))
  
  t_s = sapply(1:B, function(x) {
    x_s = x_new[sample(1:length(x_new),length(x_new),replace=T)]
    y_s = y_new[sample(1:length(y_new),length(y_new),replace=T)]
    mean(x_s) - mean(y_s) / (sd(x_s)**2/length(x_s)+sd(y_s)**2/length(y_s))
  })
  
  p_val = sum(t_s[abs(t_s)>t])/B
}


res = sapply(, function(x) {
  microbenchmark(sample_with_repl(sample_with_repl(runif(x),x)),times=100)
})
mean_res = sapply(1:15, function(x) {
  mean(unlist(res[2,x]))
})