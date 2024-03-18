# helper function used in FeatExtractClassify_FDA.R

fda <- function(X) {
  xjbar <- matrix(0, nrow = 100, ncol = 40)
  for (i in 1:100) {
    for (j in 1:40) {
      xjbar[i, j] <- mean(X[i, ((j - 1) * 5 + 1):(j * 5)])
    }
  }
  
  xbar <- apply(xjbar, 1, mean)
  
  SB <- matrix(0, nrow =100, ncol = 100)
  SW <- matrix(0, nrow = 100, ncol = 100)
  for (j in 1:40) {
    SB <- SB + tcrossprod(xjbar[, j] - xbar)
    for (i in 1:5) {
      SW <- SW + tcrossprod(X[, i] - xjbar[, j])
    }
  }
  
  SW <- SW + 1e-6 * diag(ncol(SW)) # adding some noise to avoid singularity
  
  eig_result <- geigen::geigen(SB, SW)
  lambda <- eig_result$values
  V <- eig_result$vectors
  
  ordered_index <- order(lambda, decreasing = TRUE)
  V <- V[, ordered_index]
  
  return(V)
}