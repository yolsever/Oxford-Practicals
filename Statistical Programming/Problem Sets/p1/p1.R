# 1a
seq(1,21,2)
# 1b
seq(50,11,-3)
# 1c
c(1,2**seq(1,10))
# 1d
matrix(seq(1,16),nrow=4,ncol=4,byrow=TRUE)

# 2a
b = rep(c(1,2,3,4),each=5)
# 2b
c = sample(b)
# 2c
c[c==1] = "A"
c[c==2] = "B"
c[c==3] = "C"
c[c==4] = "D"
c

# 3a
vec = sample(c(1,-1), 25, replace=TRUE, prob=c(1/2,1/2))
# 3b
cumsum(vec)
# 3c
plot(cumsum(sample(c(1,-1), 1000, replace=TRUE, prob=c(1/2,1/2))), type = "l")
# 3d
n=25
2*rbinom(1, n, 0.5)-n

# FIX: 3e
n=25
2*rbinom(1000, n, 0.5)-n

# Estimate the probability of exceeding 10
sum((2*rbinom(1000, n, 0.5)-n)>10)/1000

# FIX: 3f
pbinom(10, 1000, 1/2)

# 4a
D = diag(seq(1:10)**-1)

# 4b
U = matrix(-rep(1,100), nrow=10,ncol= 10, byrow= TRUE)
U[row(U) == col(U)] = 4

# 4c

sqrt(sum(U[,1]**2))

for (i in 1:10) {
  U[,i]= U[,i] / sqrt(sum(U[,i]**2))
}

# U*U
colSums(U*U)
# FIX
# stopifnot(U)
# identical()

# 4d

X = U*D*t(U)

# 4e

eigen(X)

# 4f

# 5a
i = 1
x = 0.3
x = 0.1 + 0.1 + 0.1
b = c()

while (x > 0 & i <= 80) {
  y = 2 * x
  if (y >= 1) {
    b[i] = 1
  }
  else {
    b[i] = 0
  }
  x = y - b[i]
  if (x == 0) {
    break
  }
  i = i + 1
}

# 5b


