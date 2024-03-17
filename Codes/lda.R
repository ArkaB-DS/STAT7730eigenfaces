# helper function used in FeatExtractClassify_FDA.R

lda <- function(X) {
  # Compute class means
  muj <- matrix(0, nrow = 100, ncol = 40)
  for (i in 1:100) {
    for (j in 1:40) {
      muj[i, j] <- mean(X[i, ((j - 1) * 5 + 1):(j * 5)])
    }
  }
  
  # Compute overall mean
  mu <- apply(muj, 1, mean)
  
  # Compute scatter matrices
  SB <- matrix(0, nrow = 100, ncol = 100)
  SW <- matrix(0, nrow = 100, ncol = 100)
  for (j in 1:40) {
    SB <- SB + tcrossprod(muj[, j] - mu)
    for (i in 1:5) {
      SW <- SW + tcrossprod(X[, i] - muj[, j])
    }
  }
  
  # Regularize SW to ensure positive definiteness
  SW <- SW + 1e-6 * diag(ncol(SW))
  
  # Perform generalized eigenvalue decomposition
  eig_result <- geigen::geigen(SB, SW)
  lambda <- eig_result$values
  V <- eig_result$vectors
  
  # Order eigenvalues and eigenvectors
  idx <- order(lambda, decreasing = TRUE)
  lambda <- lambda[idx]
  V <- V[, idx]
  
  return(list(V = V, lambda = lambda))
}