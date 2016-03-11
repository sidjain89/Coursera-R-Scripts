library(datasets)
library(caret)
library(ggplot2)
data("iris")

# create training and validation sets
inTrain <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainset <- iris[inTrain,]
testset <- iris[-inTrain,]

# see the distributions
qplot(trainset$Petal.Width, trainset$Sepal.Width, colour = trainset$Species)

# fit the model with the training data
fit <- train(Species ~ ., method = "rpart", data = trainset)
fit$finalModel
fit$results

getTrainPerf(fit) # train perf

# Visualize via dendo gram
plot(fit$finalModel, uniform = TRUE, main = "Classification Tree")
text(fit$finalModel, use.n = TRUE, all =  TRUE, cex = 0.8)

# this is a more cool visualiztion
library(rattle)
fancyRpartPlot(fit$finalModel, main = "decision tree")

# predict on test set
pred <- predict(fit, newdata = testset)
confusionMatrix(pred, testset$Species)