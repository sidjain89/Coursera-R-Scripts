library(ISLR)
library(caret)
library(ggplot2)

data("Wage")
set.seed(12345)

# remove 3 non useful columns - sex = only males, 
# region = only middle atlantic and dont need log of wage for pred
wg <- subset(Wage, select = -c(logwage, region, sex)) 
inTrain <- createDataPartition(wg$wage, p = 0.7, list = FALSE)
tr <- wg[inTrain,]
ts <- wg[-inTrain,]

# explore the data
summary(wg)
nrow(wg)
ncol(wg)

qplot(age, wage, colour = jobclass, data = tr)
qplot(age, wage, colour = education, data = tr)

# fit the model
fit <- train(wage ~ ., method = "glm", data = tr)

pred <- predict(fit, newdata = ts)

confusionMatrix(pred, tr$wage)
confusionMatrix(pred, ts$wage)