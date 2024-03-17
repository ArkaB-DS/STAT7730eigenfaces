# helper function used in FeatExtractClassify_PCA.R

classifier <- function(Ytr, Yts) {
  # Get dimensions of Ytr and Yts
  m <- ncol(Ytr)
  n <- ncol(Yts)
  
  # Initialize d matrix
  d <- matrix(0, nrow = n, ncol = m)
  
  # Compute squared Euclidean distances
  for (i in 1:n) {
    for (j in 1:m) {
      d[i, j] <- sum((Yts[, i] - Ytr[, j])^2)
    }
  }
  
  # Find minimum values and corresponding indices
  minval <- apply(d, 1, min)
  index <- apply(d, 1, which.min)
  
  return(index)
}
