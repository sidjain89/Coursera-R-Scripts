library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

head(training)

sb <- subset(training, select = c(IL_11, IL_13, IL_16, IL_17E, IL_1alpha,
                                  IL_3, IL_4, IL_5, IL_6, IL_6_Receptor,
                                  IL_7, IL_8))

pca <- preProcess(sb[], method = c("pca"),
                  thresh = 0.8)

head(pca)

pcaTr <- predict(pca, sb)
head(pcaTr)

names <- colnames(training)
ls <- c("-1")

for (name in names){
  x <- substr(name, 1,2)
  # print(x)
  if(x == "IL"){
    print(name)
  }
}

tr1 <- subset(training, select = c(IL_11, IL_13, IL_16, IL_17E, IL_1alpha,
                                   IL_3, IL_4, IL_5, IL_6, IL_6_Receptor,
                                   IL_7, IL_8, diagnosis))

pcaTr$diagnosis <- tr1$diagnosis
head(pcaTr)


# now predict
fit1 <- train(diagnosis ~ ., data = tr1, method = "glm")
fit2 <- train(diagnosis ~ ., data = pcaTr, method = "glm")

pred1 <- predict(fit1, tr1)
pred2 <- predict(fit2, pcaTr)

confusionMatrix(pred1, tr1$diagnosis)[3][1]
confusionMatrix(pred2, pcaTr$diagnosis)[3][1]







