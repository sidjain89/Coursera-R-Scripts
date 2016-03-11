library(stats)
library(caret)
data("faithful")
set.seed(12345)

inTrain <- createDataPartition(faithful$waiting, p = 0.7, list = FALSE)
train_data <- faithful[inTrain,]
test_data <- faithful[-inTrain,]

lm1 <- lm(train_data$eruptions ~ train_data$waiting)
summary(lm1)

plot(x = train_data$eruptions, y = train_data$waiting, xlab = "duration", 
     ylab = "waiting", col = "blue", pch = 20 )
lines(train_data$waiting, lm1$fitted.values, lwd = 3)
lm1$coefficients

lm1$coefficients[1] + lm1$coefficients[2]

fit <- train(waiting ~ ., data = train_data, method = "glm")
pred <- predict(fit, newdata = test_data)
summary(pred)
confusionMatrix(pred, train_data$waiting)
confusionMatrix(pred, test_data$waiting)