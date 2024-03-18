# helper function used in FeatExtractClassify_FDA.R

lda <- function(X) {
  muj <- matrix(0, nrow = 100, ncol = 40)
  for (i in 1:100) {
    for (j in 1:40) {
      muj[i, j] <- mean(X[i, ((j - 1) * 5 + 1):(j * 5)])
    }
  }
  
  mu <- apply(muj, 1, mean)
  
  SB <- matrix(0, nrow =100, ncol = 100)
  SW <- matrix(0, nrow = 100, ncol = 100)
  for (j in 1:40) {
    SB <- SB + tcrossprod(muj[, j] - mu)
    for (i in 1:5) {
      SW <- SW + tcrossprod(X[, i] - muj[, j])
    }
  }
  
  SW <- SW + 1e-6 * diag(ncol(SW)) # adding some noise to avoid singularity
  
  eig_result <- geigen::geigen(SB, SW)
  lambda <- eig_result$values
  V <- eig_result$vectors
  
  idx <- order(lambda, decreasing = TRUE)
  V <- V[, idx]
  
  return(V)
}