# 1a
replicate(50,rt(10,5))



# 1b

mapply(rt,n=10,df=1:50)

# 1c

lapply(1:20,seq)

# 1d

set.seed(2020)
X <- matrix(rexp(200), 20, 10)

apply(X, 2 ,min)


# FIX: 1e
apply(CO2,2 ,is.numeric)

# 2a
system.time(read.table("C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/GTEx_analysis.txt.gz",header=T,skip=3)
)
# 2b
system.time(data.table::fread("C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/GTEx_analysis.txt.gz",skip=1)
)
# 2c
dt <- data.table::fread("C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/GTEx_analysis.txt.gz",skip=1)

system.time(save(dt, file="C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/2c"))
system.time(data.table::fwrite(dt, file="C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/2c"))

# 3a
spr = data.table::fread("C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/sprays.txt", stringsAsFactors = T)
head(spr)
str(spr)
# 3b
str(spr)
# FIX: 3c - add vector operations


tapply(spr$count,spr$spray, mean)

# 3d
tapply(spr$count,spr$spray, probs=c(0.25,0.75), quantile)

# 4a
max_ = apply(dt[,c(-1,-2)],1, function(x) {
  sample(names(x)[x == max(x)],1)
})

summary(max_)
max(max_)

# 4b

sapply(dt[,c(-1,-2)], function(x) {

  c("mean" = mean(x),
    "std" = sd(x),
    "median" = median(x),
    "quants" = quantile(x,probs=c(0.05,0.95)))
})

# ASK: 4c

# apply(dt[Description != "APOB",c(-1,-2)],1,function(x){
#   sum((x - as.numeric(dt[Description =="APOB",c(-1,-2)]))**2)
#   # sum((x - focal_entry)**2)
# })



focal_entry <- as.numeric(dt[as.vector(dt[, 2] == "APOB"), -c(1:2)])

system.time({
  results <- apply(dt[, -c(1:2)], 1, function(x){
    y <- x - focal_entry
    sum(y ** 2)
  })
})
