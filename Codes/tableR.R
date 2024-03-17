data7730 <- data.frame(
  k = 1:40,
  PCA_k = PCA_as_a_function_of_k,
  FDA_k = FDA_as_a_function_of_k,
  SP_k = SP_as_a_function_of_k
)

# Save the data frame to a CSV file
write.csv(data7730, file = "results.csv", row.names = FALSE)
