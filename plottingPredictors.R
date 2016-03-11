library(ISLR)
library(ggplot2)
library(caret)

data(Wage)
summary(Wage)

inTr <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
train_data <- Wage[inTr,]
test_data <- Wage[-inTr,]

# use caret
featurePlot(x = train_data$age, y = train_data$wage, labels = c("age", "wage"))

#use ggplot2
qplot(x = train_data$age, y = train_data$wage, xlab = "age", 
      ylab = "wage", colour = train_data$jobclass)

# add regression smoothners
qq <-qplot(age, wage, data = train_data, colour = education)
qq + geom_smooth(method = "lm", formula = y ~ x)

# view bins
library(Hmisc)
cutWage <- cut2(train_data$wage, g = 4)
table(cutWage)

#box plots/jitter/densityplot with cut2
p1 <- qplot(cutWage, age, data = train_data, fill = cutWage, geom = c("boxplot"))
p1

p2 <- qplot(cutWage, age, data = train_data, fill = cutWage, geom = c("jitter"))
p2

#this will show where the bulk of the data is
p3 <- qplot(wage, colour = education, data = train_data, geom = "density")
p3



#tables
tbl1 <- table(cutWage, train_data$jobclass)
prop.table(tbl1, margin = 2)


