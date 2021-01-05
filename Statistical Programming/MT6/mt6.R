library(bnlearn)
library("testthat")

# 1.1
# Pseudocode for random digraph generation
# melancon <- function(V) {
#   for i in 1:n_draw
#     draw a couple of integers unifly at random (i,j)
#     If (i,j) in x_t
#       X_{t+1} = X_t \ (i,j)
#     Else
#       if X_t + (i,j) acyclic
#         X_{t+1} = X_t + (i,j)
#       else
#         X_{t+1} = X_t
#   return(X_final)
# }

# 1.2

melancon <- function(nodes, n) {
  G = empty.graph(nodes)
  if (n==0 | length(nodes) == 1) {
    return(G)
  }
  for (i in 1:n) {
    pick = c(nodes[sample(1:length(nodes), 1)], 
             nodes[sample(1:length(nodes), 1)])
    if (pick[1] == pick[2]){
      next
    }
    if (amat(G)[pick[1],pick[2]] == 1) {
      amat(G)[pick[1],pick[2]] = 0
    }
    else {
      if(path(G, pick[2],pick[1]))
      {
        next
      }
      else {
        amat(G)[pick[1],pick[2]] = 1
      }
    }
  }
  return(G)
}

# 1.3
plot(melancon(letters[1:5],100))

plot(melancon(letters[1:7],1000))

# 1.4
# DFS or BFS take O(|V| + |E|)
# In our case |E| = |V|**4/2
# So computational complexity is O(n*|V|^2)
# Space complexity is O(|V|^2) bc of the adjacency matrix

# 1.5
dt = as.data.table(merge(letters,LETTERS))
dt[,m := paste0(x,y)]

# Time complexity plot wrt nodes
times_node = lapply(1:length(dt$m),function(x) {
  system.time(melancon(dt$m[1:x],20))["elapsed"]
}
)
plot(unlist(times_node))

# Time complexity plot wrt n
times_n = lapply(1:1000,function(x) {
  system.time(melancon(letters[1:10],x))["elapsed"]
}
)
plot(unlist(times_n))

# 1.6
# Time complexity for the adjacency list version is still O(|V|^2)
# Space complexity is still O(|V|^2)

# 1.7 Validation function

# 1.8 Unit tests
expect_equal(melancon(letters, 0), empty.graph(letters))
expect_equal(melancon(letters[1], 10000), empty.graph(letters[1]))
expect_error(melancon(1,1))
expect_error(melancon("",10))
expect_error(melancon(c(),10))


# 1.9
test_that("can draw dags from uniform probability for small number of nodes",
          {
            set.seed(2020)
            dags <- sapply(1:2000,function(iRep) {
              dag <- melancon(LETTERS[1:3], n = 25)
              modelstring(dag)
            })
            
            normalized_dag_frequency <- table(dags) / length(dags)
            
            expect_true(min(normalized_dag_frequency) >= 0.02)
            expect_true(max(normalized_dag_frequency) <= 0.05)
            
            plot(normalized_dag_frequency, ylim = c(0, 0.05))
            abline(h = 0.04, col = 2, lwd = 2, lty = 2)
            }
          )

