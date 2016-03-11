library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type, p =0.75, list=FALSE)
train <-spam[inTrain,]
test <- spam[-inTrain,]
set.seed(12345)
modelFit <- train(type~., data = train, method = "glm")
pred <- predict(modelFit, newdata = test)
confusionMatrix(pred, test$type)
pred1 <- predict(modelFit, newdata = train)
confusionMatrix(pred1, train$type)
# typeof(confusionMatrix(pred1, train$type))
x <- confusionMatrix(pred1, train$type)
x[1:2][2][1]

set.seed(12345)
folds <- createFolds(y=spam$type, k = 5, list = TRUE, returnTrain = FALSE)

# check the length of each fold
sapply(folds, length)

head(folds[1])

