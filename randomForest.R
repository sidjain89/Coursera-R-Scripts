library(datasets)
library(caret)
library(ggplot2)
data("iris")

# create training and validation sets
inTrain <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainset <- iris[inTrain,]
testset <- iris[-inTrain,]

# fit the model with the training data
fit <- train(Species ~ ., method = "rf", data = trainset, prox = TRUE)
fit$results

# training performance
getTrainPerf(fit)

# view a particular tree
getTree(fit$finalModel, k = 500) # run fit$finalModel to see the total no of trees

# apply the model to the test set
pred <- predict(fit, newdata = testset)
confusionMatrix(testset$Species, pred)
table(pred, testset$Species)

#visualize the predictions
testset$predRight <- pred == testset$Species # get those sets which were correctly predicted
qplot(testset$Sepal.Width, testset$Sepal.Length, colour = testset$predRight, main = "test data predictions")
qplot(testset$Petal.Length, testset$Petal.Width, colour = testset$predRight, main = "test data predictions")