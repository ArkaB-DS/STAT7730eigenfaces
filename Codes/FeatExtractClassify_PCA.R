U <- svd(Ytrain)$u

Index <- classifier(Ytrain, Ytest)

PCA_as_a_function_of_k <- vector(length = 40)
for (k in 1:40) {
  counter_PCA <- 0
  for (m in 1:1000) {
    U1_PCA <- U[, 1:k]
    Y1_PCA <- crossprod(U1_PCA, Ytrain)
    random_idx_PCA <- sample(1:200, 1)
    I_PCA <- Ytest[, random_idx_PCA]
    I1_PCA <- crossprod(U1_PCA, I_PCA)
    true_index_PCA <- Index[random_idx_PCA]
    predicted_idx_PCA <- classifier(Y1_PCA, I1_PCA)
    if (ceiling(predicted_idx_PCA/5) == ceiling(true_index_PCA/5))
      counter_PCA <- counter_PCA + 1
  }
  PCA_as_a_function_of_k[k] <- counter_PCA / 1000*100
}