# helper function used in FeatExtractClassify_PCA.R

nearest_neighbour_identifier <- function(Ytr, Yts) {
  m <- ncol(Ytr)
  n <- ncol(Yts)
  
  d <- matrix(0, nrow = n, ncol = m)
  
  for (i in 1:n) {
    for (j in 1:m) {
      d[i, j] <- sum((Yts[, i] - Ytr[, j])^2)
    }
  }
  
  index <- apply(d, 1, which.min)
  return(index)
}
