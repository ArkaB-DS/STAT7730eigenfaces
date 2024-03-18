data7730 <- data.frame(
  k = 1:40,
  PCA = PCA_as_a_function_of_k,
  FDA = FDA_as_a_function_of_k,
  SP = SP_as_a_function_of_k
)

write.csv(data7730, file = "results.csv", row.names = FALSE)