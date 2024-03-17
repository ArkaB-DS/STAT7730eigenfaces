# Plots of F(k) vs k
pdf("./Plots/Plot_of_Fk_v_k.pdf", height = 8, width = 10)

plot(PCA_as_a_function_of_k, type="o", col = 'red', xlab = 'k', ylab = 'F(k)',
     main = 'Variation of F(k) versus k', lwd=2)

lines(SP_as_a_function_of_k, col = 'blue', lwd=2, type= "o")

lines(FDA_as_a_function_of_k, col = 'green', type= "o", lwd=2)

legend("topleft", legend = c("PCA", "FDA", "SP"), lwd=2, 
       col = c("red","green", "blue"), pch = 16)

dev.off()