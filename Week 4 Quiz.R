library(ElemStatLearn)

data(vowel.train)

data(vowel.test)

fit_dt <- train(nicu_nwb ~ ., method = "rpart", data = trainset) # decision tree
fit_nb <- train(nicu_nwb ~ ., method = "nb", data = trainset) # naive bayes
fit_lr <- train(nicu_nwb ~ ., method = "lm", data = trainset) #logistic regression