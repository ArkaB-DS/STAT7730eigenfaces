U_SP <- diag(644)
SP_as_a_function_of_k <- vector(length = 40)
for (k in 1:40) {
  counter_SP <- 0
  for (m in 1:1000) {
    U1_SP <- U_SP[, 1:k]
    Y1_SP <- crossprod(U1_SP, Ytrain)
    random_idx_SP <- sample(1:200, 1)
    I_SP <- Ytest[, random_idx_SP]
    I1_SP <- crossprod(U1_SP, I_SP)
    true_index_SP <- Index[random_idx_SP]
    predicted_idx_SP <- nearest_neighbour_identifier(Y1_SP, I1_SP)
    if (ceiling(predicted_idx_SP/5) == ceiling(true_index_SP/5)) {
      counter_SP <- counter_SP + 1
    }
  }
  SP_as_a_function_of_k[k] <- counter_SP / 1000*100
}
