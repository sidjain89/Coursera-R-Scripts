library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type, p = 0.75, list = FALSE)
train_data <- spam[inTrain,]
test_data <-spam[-inTrain,]

hist(train_data$capitalAve, main="", xlab = "avg capital run length", ylab = "Count")

dat <- c(mean(train_data$capitalAve), sd(train_data$capitalAve))
dat

# standerdize the data
train_data$capitalAve <- (train_data$capitalAve - mean(train_data$capitalAve))/sd(train_data$capitalAve)

items <- is.na(train_data$capitalTotal)
items