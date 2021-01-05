# 1a
test_that("calculate moving average works",{
  expect_equal(calculate_moving_average_for_k(x=1:10,k=3),c(1.5,4/3,2,8/3
                                                           , 10/3,4,14/3,16/3,
                                                           6,9.5))
})

# 1b
calculate_moving_average_for_k <- function(x,k) {
  new_x = c()
  for (j in 1:length(x)) {
    i_1 = max(1,j-(k-1)/2)
    i_2 = min(length(x),j+(k-1)/2)
    new_x[j] = 1/(i_2-i_1+1)*(x[i_1] + x[i_2])
  }
  return(new_x)
}

x=1:10
k=3

calculate_moving_average_for_k(x,k)

# 1c
calculate_moving_averages <- function(x,k_vec) {
  new_x = matrix(numeric(100), nrow=length(x),ncol=length(k_vec))
  for (i in 1:length(x)) {
    for (j in 1:length(k_vec)){
      i_1 = max(1,i-(k_vec[j]-1)/2)
      i_2 = min(length(x),i+(k_vec[j]-1)/2)
      new_x[i,j] = 1/(i_2-i_1+1)*(x[i_1] + x[i_2])
    }
  }
  return(new_x)
}

# 1d
test_that("acceptance test for calculate_moving_averages",{
  expect_equal(calculate_moving_averages(x=1:10,k_vec=c(1,3))[,2],c(1.5,4/3,2,8/3
                                                            , 10/3,4,14/3,16/3,
                                                            6,9.5))
  expect_equal(calculate_moving_averages(x=1:10,k_vec=c(1,3))[,1],2*(1:10))
}) 


# 1e
ans <- "I prefer writing the tests before because it allows me to rigorously test the function 
once I know what to expect for the answers" 

# 1f
test_that("Check weird cases", {
              expect_equal(calculate_moving_averages(x, k_vec=c(-Inf,Inf)), matrix(rep(-Inf,10),rep(Inf,10)))
})
  
# 2

n1 <- 200
n2 <- 100


nWeights <- 50
ref_allele_matrix <- matrix(sample(c(0, 1), n1 * n2, replace = TRUE), nrow = n1)
weight_matrix <- matrix(rnorm(n1 * n2, mean = 1, sd = 0.1), nrow = n1)
Gamma_Weights_States <- runif(nWeights)
row_update <- 1

initial_function <- function(
 ref_allele_matrix,
 fst,
 weight_matrix,
 Gamma_Weights_States,
 row_update
) {
  x1_sums <- colSums(
    weight_matrix[-row_update,]*ref_allele_matrix[-row_update,]
  )
  x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
  x1_matrix <- t(x1_matrix)
  x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
  B <- x1_matrix + x2
  S <- colSums(weight_matrix[-row_update,])
  A <- outer(S,Gamma_Weights_States, FUN='+')
  allele_frequencies <- t(B)/A
  allele_frequencies
}  

# 2a

new_function <- function(
  ref_allele_matrix,
  fst,
  weight_matrix,
  Gamma_Weights_States,
  row_update
) {
  x1_sums <- colSums(
    weight_matrix[-row_update,]*ref_allele_matrix[-row_update,]
  )
  x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
  x1_matrix <- t(x1_matrix)
  x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
  B <- x1_matrix + x2
  S <- colSums(weight_matrix[-row_update,])
  A <- outer(S,Gamma_Weights_States, FUN='+')
  allele_frequencies <- t(B)/A
  allele_frequencies
}  

# 2b

test_that("Check initial function and new function returns same values",{
  expect_equal(new_function(ref_allele_matrix,fst,weight_matrix,Gamma_Weights_States,
                            row_update), initial_function(ref_allele_matrix,fst,
              weight_matrix,Gamma_Weights_States, row_update))
})

test_that("Check initial function and new function returns same values",{
  expect_equal(new_function(ref_allele_matrix,fst,weight_matrix,Gamma_Weights_States,
                            row_update), initial_function(ref_allele_matrix+1,fst,
                                                          weight_matrix,Gamma_Weights_States, row_update))
})

# 2c
microbenchmark(new_function(ref_allele_matrix,fst,weight_matrix,Gamma_Weights_States,row_update),times=10000)
microbenchmark(initial_function(ref_allele_matrix,fst,weight_matrix,Gamma_Weights_States,row_update),times=10000)

# 2d
profvis({
  for (i in 1:1000){
  x1_sums <- colSums(
    weight_matrix[-row_update,]*ref_allele_matrix[-row_update,]
  )
  x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
  x1_matrix <- t(x1_matrix)
  x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
  B <- x1_matrix + x2
  S <- colSums(weight_matrix[-row_update,])
  A <- outer(S,Gamma_Weights_States, FUN='+')
  allele_frequencies <- t(B)/A
}})
