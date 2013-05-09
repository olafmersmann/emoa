library("emoa")

dominationMatrix = function(X){ 
  n = ncol(X)
  dom = matrix(FALSE, ncol = n, nrow = n)
  sapply(1:(n-1), function(i) sapply((i+1):n, function(j){
    tmp = is_dominated(X[,c(i, j)])
    dom[i, j] <<- tmp[2]
    dom[j, i] <<- tmp[1]
  }))
  dom
}

n <- 100
x <- matrix(runif(2 * n), ncol=n)
r <- dominance_matrix(x)


