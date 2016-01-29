library(ISLR)
library(ggplot2)
library(caret)
data("Wage")
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p = 0.75, list = F)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

featurePlot(x = training[,c("age", "education", "jobclass")], y=training$wage, plot = "pairs")
