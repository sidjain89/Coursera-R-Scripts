#################### Q1 ######################################################################
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

set.seed(125)
inTrain <- createDataPartition(segmentationOriginal$Case, p = 0.7, list = FALSE)
trainset <- segmentationOriginal[segmentationOriginal$Case=="Train",][, -2]
testset <- segmentationOriginal[segmentationOriginal$Case=="Test",][, -2]

mod_cart <- train(Class ~ ., data = trainset, method = "rpart")
getTrainPerf(mod_cart)
library(rattle)
fancyRpartPlot(mod_cart$finalModel)

########################### Q3 ###############################################################
library(pgmm)
data(olive)
olive = olive[,-1]


set.seed(125)
inTrain <- createDataPartition(olive$Area, p = 0.7, list = FALSE)
trainset <- olive[inTrain,]
testset <- olive[-inTrain,]

mod_cart <- train(Area ~ ., data = trainset, method = "rpart")
val <- predict(mod_cart, newdata = as.data.frame(t(colMeans(olive))))

val

########################### Q4 ###############################################################
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

trainSA$chd <- as.factor(trainSA$chd)
testSA$chd <- as.factor(testSA$chd)


set.seed(13234)

mod_glm <- train(chd ~ ., data = trainSA, method = "glm", family = "binomial")
misclass<- 1 - as.numeric(getTrainPerf(mod_glm)[1][,1])
misclass

pred <- predict(mod_glm, newdata = testSA)
confusionMatrix(testSA$chd, pred)


########################### Q5 ###############################################################
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vtr <- vowel.train
vte <- vowel.test

vtr$y <- as.factor(vtr$y)
vte$y <- as.factor(vte$y)

set.seed(33833)
mod_rf <- train(y ~ ., data = vtr, method = "rf", prox = TRUE)
pred <- predict(mod_rf, vte)

getTrainPerf(mod_rf)
confusionMatrix(vte$y, pred)

varImp(mod_rf)

#########################################################################################

