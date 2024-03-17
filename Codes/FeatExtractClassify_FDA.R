# Computing the FDA of Ytrain
d <- 200/2
U0 <- U[, 1:d]
Ytrain_new <- crossprod(U0, Ytrain)#t(U0) %*% Ytrain
lda_result <- lda(Ytrain_new)
V <- lda_result$V
V <- far::orthonormalization(V, basis = FALSE, norm = TRUE) 
FDA_as_a_function_of_k <- vector(length = 40)
for (k in 1:40) {
 counter_FDA <- 0
  for (m in 1:500) {
    V1_FDA <- V[, 1:k]
    U1_FDA <- U0 %*% V1_FDA
    Y1_FDA <- crossprod(U1_FDA, Ytrain)#t(U1) %*% Ytrain
    random_idx_FDA <- sample(1:200, 1)
    I_FDA <- Ytest[, random_idx_FDA]
    I1_FDA <- crossprod(U1_FDA, I_FDA)#t(U1) %*% I
    true_index_FDA <- Index[random_idx_FDA]
    predicted_idx_FDA <- classifier(Y1_FDA, I1_FDA)
    if (predicted_idx_FDA == true_index_FDA)
      counter_FDA <- counter_FDA + 1
  }
 FDA_as_a_function_of_k[k] <- counter_FDA / 500
}