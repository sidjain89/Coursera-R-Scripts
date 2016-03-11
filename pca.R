library(caret)
library(kernlab)
library(ggplot2)
data(spam)
inTrain <- createDataPartition(y=spam$type, p = 0.75, list = FALSE)
train_data <- spam[inTrain,]
test_data <-spam[-inTrain,]

M <- abs(cor(train_data[,-58]))
diag(M) <- 0
which(M > 0.8, arr.ind = T)

smallSpam <- spam[,c(34,32)]
pca <- prcomp(smallSpam)
plot(pca$x[,1], pca$x[,2])
pca

qplot(x = pca$x[,1], y = pca$x[,2], xlab = "num415", 
      ylab = "num857")