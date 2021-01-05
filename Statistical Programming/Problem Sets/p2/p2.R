library(stats)
# 1a
summarize <- function(x) {
  
  return(c("mean"=mean(x), "variance"=var(x), "range" =range(x)))
  
}


summarize(runif(30))


# CHECK: 1b 

poiss <- function(x,n) {
  s <- 0
  for (i in 0:n){
    s <- s + x**i/factorial(i)
  }
  return(exp(-x)*s)
}


poiss(3,5)

# 1c

char_ <- function(x) {
  for (i in x) {
    
    if(is.character(i)) {
      print(i)
    }
  }
}


# 1d
# last = 0 
randw_k <- function(k){
  last = 0
  i = 1
  while(abs(last) != k){
    vec = cumsum(sample(c(1,-1), i, replace=TRUE, prob=c(1/2,1/2)))
    last = vec[length(vec)]
    i = i + 1
  }
  plot(vec, type="l")
}

