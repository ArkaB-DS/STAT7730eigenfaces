pdf("./Plots/eigenfaces_plot.pdf", width = 8, height = 8)

library(reshape2)

Index <- nearest_neighbour_identifier(Ytrain, Ytest)  

set.seed(3)
three_random_indices <- sample(1:200, 3)


par(mfrow = c(2, 3))
for (i in 1:3) {
  I <- matrix(Ytrain[, Index[three_random_indices[i]]], nrow = 28)
  image(t(I)[, nrow(I):1], col = gray.colors(256), asp = 1, axes = FALSE)
  title(paste("Train Face", i))
}

for (i in 1:3) {
  I <- matrix(Ytest[, three_random_indices[i]], nrow = 28)
  image(t(I)[, nrow(I):1], col = gray.colors(256), asp = 1, axes = FALSE)
  title(paste("Test Face", i))
}
dev.off()
