library(caret);
library(kernlab);library(randomForest)
data(spam);
inTrain <- createDataPartition(y=spam$type, p= 0.75, list = FALSE);
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

#fittinga a model
set.seed(34420)
modelFit <- train(type ~., data = training, metod = "lm")
modleFit

mod1 = glm(type ~., data = training, family=binomial)
summary(mod1)

pred <- predict(mod1, newdata = testing, type = 'response')
pred
table(test$not.fully.paid, pred > 0.5)

pred <- predict(mod1, newdata = testing)
pred
