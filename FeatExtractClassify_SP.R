# Computing the Simple Projection of Ytrain
U_SP <- diag(644)
SP_as_a_function_of_k <- vector(length = 40)
for (k in 1:40) {
  counter_SP <- 0
  for (m in 1:500) {
    U1_SP <- U_SP[, 1:k]
    Y1_SP <- crossprod(U1_SP, Ytrain)#t(U1) %*% Ytrain
    random_idx_SP <- sample(1:200, 1)
    I_SP <- Ytest[, random_idx_SP]
    I1_SP <- crossprod(U1_SP, I_SP)#t(U1) %*% I
    true_index_SP <- Index[random_idx_SP]
    predicted_idx_SP <- classifier(Y1_SP, I1_SP)
    if (predicted_idx_SP == true_index_SP)
      counter_SP <- counter_SP + 1
  }
  SP_as_a_function_of_k[k] <- counter_SP / 500
}
