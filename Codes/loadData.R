Ytrain <- as.matrix(read.table("./Data/traindata.txt"))
Ytest <- as.matrix(read.table("./Data/testdata.txt"))

Ytrain <- scale(Ytrain)
Ytest <- scale(Ytest)