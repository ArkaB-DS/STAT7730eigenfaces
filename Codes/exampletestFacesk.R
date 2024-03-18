set.seed(11)

i <- sample(1:200, 1)
random_ks <- c(10, 25, 40)
PCA_recognition <- numeric(3)
FDA_recognition <- numeric(3)
SP_recognition <- numeric(3)
predicted_idx_PCA <- numeric(3)
predicted_idx_FDA <- numeric(3)
predicted_idx_SP <- numeric(3)

I_ <- Ytest[, i]
true <- Index[i]
counter <- 1
for(k in random_ks){  
  #PCA
  U1_PCA <- U[, 1:k]
  Y1_PCA <- crossprod(U1_PCA, Ytrain)
  I1_PCA <- crossprod(U1_PCA, I_)
  predicted_idx_PCA[counter] <- classifier(Y1_PCA, I1_PCA)
  PCA_recognition[counter] <- (ceiling(predicted_idx_PCA[counter]/5) == ceiling(true/5))
  #FDA
  V1_FDA <- V[, 1:k]
  U1_FDA <- U0 %*% V1_FDA
  Y1_FDA <- crossprod(U1_FDA, Ytrain)
  I1_FDA <- crossprod(U1_FDA, I_)
  predicted_idx_FDA[counter] <- classifier(Y1_FDA, I1_FDA)
  FDA_recognition[counter] <- (ceiling(predicted_idx_FDA[counter]/5) == ceiling(true/5))
  #SP
  U1_SP <- U_SP[, 1:k]
  Y1_SP <- crossprod(U1_SP, Ytrain)
  I1_SP <- crossprod(U1_SP, I_)
  predicted_idx_SP[counter] <- classifier(Y1_SP, I1_SP)
  SP_recognition[counter] <- (ceiling(predicted_idx_SP[counter]/5) == ceiling(true/5))
  counter <- counter + 1
  }

pdf("./Plots/test_train_example_plots.pdf", width = 8, height = 8)
par(mfcol = c(3, 4))

#test images
for (l in 1:3) {
  I <- matrix(I_, nrow = 28)
  image(t(I)[, nrow(I):1], col = gray.colors(256), asp = 1, axes = FALSE)
  title(paste("Test Person", ceiling(i/5)))
}

#PCA column
  I <- matrix(Ytrain[, predicted_idx_PCA[1]], nrow = 28)
  image(t(I)[, nrow(I):1], col = gray.colors(256), asp = 1, axes = FALSE)
  title(paste("PCA, k=", 10, "Person", ceiling(predicted_idx_PCA[1]/5)))
  
  I <- matrix(Ytrain[, predicted_idx_PCA[2]], nrow = 28)
  image(t(I)[, nrow(I):1], col = gray.colors(256), asp = 1, axes = FALSE)
  title(paste("PCA, k=", 25, "Person", ceiling(predicted_idx_PCA[2]/5)))

  I <- matrix(Ytrain[, predicted_idx_PCA[3]], nrow = 28)
  image(t(I)[, nrow(I):1], col = gray.colors(256), asp = 1, axes = FALSE)
  title(paste("PCA, k=", 40, "Person", ceiling(predicted_idx_PCA[3]/5)))

# FDA Column    
  I <- matrix(Ytrain[, predicted_idx_FDA[1]], nrow = 28)
  image(t(I)[, nrow(I):1], col = gray.colors(256), asp = 1, axes = FALSE)
  title(paste("FDA, k=", 10, "Person", ceiling(predicted_idx_FDA[1]/5)))
  
  I <- matrix(Ytrain[, predicted_idx_FDA[2]], nrow = 28)
  image(t(I)[, nrow(I):1], col = gray.colors(256), asp = 1, axes = FALSE)
  title(paste("FDA, k=", 25, "Person", ceiling(predicted_idx_FDA[2]/5)))
  
  I <- matrix(Ytrain[, predicted_idx_FDA[3]], nrow = 28)
  image(t(I)[, nrow(I):1], col = gray.colors(256), asp = 1, axes = FALSE)
  title(paste("FDA, k=", 40, "Person", ceiling(predicted_idx_FDA[3]/5)))
  
# SP  column
  
  I <- matrix(Ytrain[, predicted_idx_SP[1]], nrow = 28)
  image(t(I)[, nrow(I):1], col = gray.colors(256), asp = 1, axes = FALSE)
  title(paste("SP, k=", 10, "Person", ceiling(predicted_idx_SP[1]/5)))
  
  I <- matrix(Ytrain[, predicted_idx_SP[2]], nrow = 28)
  image(t(I)[, nrow(I):1], col = gray.colors(256), asp = 1, axes = FALSE)
  title(paste("SP, k=", 25, "Person", ceiling(predicted_idx_SP[2]/5)))
  
  I <- matrix(Ytrain[, predicted_idx_SP[3]], nrow = 28)
  image(t(I)[, nrow(I):1], col = gray.colors(256), asp = 1, axes = FALSE)
  title(paste("SP, k=", 40, "Person", ceiling(predicted_idx_SP[3]/5)))
dev.off()