library(ISLR)
library(ggplot2)
library(caret)
library(klaR)
library(MASS)
data(Wage)

# remoe the logwage column
wg <- subset(Wage, select = -c(logwage))

# split the data into training and validation sets
inTrain = createDataPartition(y = wg$wage, p = 0.7, list = FALSE)
trainset <- wg[inTrain,]
testset <- wg[-inTrain,]

# create the model
fit <- train(wage ~ ., method = "gbm", data = trainset, verbose = FALSE) # with gb make sure verbose is FALSE
getTrainPerf(fit)

fit_nb <- train(wage ~ ., method = "nb", data = trainset) 
getTrainPerf(fit_nb)

pred <- predict(fit, newdata = testset)

# visualize
qplot(x = pred, y = wage, data = testset)