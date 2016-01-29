library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

suppressMessages(library(dplyr))
suppressMessages(library(Hmisc))
suppressMessages(library(gridExtra))
training <- mutate(training, index=1:nrow(training))
cutIndex <- cut2(training$index, g=10)
breaks <- 10
#byCement <- qplot(index, CompressiveStrength, data=training, color=cut2(training$Cement, g=breaks))
#byBlastFurnaceSlag <- qplot(index, CompressiveStrength, data=training, color=cut2(training$BlastFurnaceSlag, g=breaks))
#byFlyAsh <- qplot(index, CompressiveStrength, data=training, color=cut2(training$FlyAsh, g=breaks))
#byWater <- qplot(index, CompressiveStrength, data=training, color=cut2(training$Water, g=breaks))
#bySuperplasticizer <- qplot(index, CompressiveStrength, data=training, color=cut2(training$Superplasticizer, g=breaks))
#byCoarseAggregate <- qplot(index, CompressiveStrength, data=training, color=cut2(training$CoarseAggregate, g=breaks))
#byFineAggregate <- qplot(index, CompressiveStrength, data=training, color=cut2(training$FineAggregate, g=breaks))
#byAge <- qplot(index, CompressiveStrength, data=training, color=cut2(training$Age, g=breaks))
#grid.arrange(byCement, byBlastFurnaceSlag, byFlyAsh, byWater, bySuperplasticizer, byCoarseAggregate, byFineAggregate, byAge)
qplot(index, CompressiveStrength, data=training, color=cut2(training$Cement, g=breaks))


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

library(ggplot2)
library(caret)
ncol(training)
which(sapply(adData,class)=="factor")
summary(training$diagnosis)
training$diagnosis = as.numeric(training$diagnosis)
p <- prcomp(training[,grep('^IL',names(training))])
p$rotation[,1:7]
qplot(1:length(p$sdev),p$sdev / sum(p$sdev))
which(cumsum(p$sdev) / sum(p$sdev) <= .9)
(cumsum(p$sdev) / sum(p$sdev))[8]
#Result here
preProc <- preProcess(training[,grep('^IL',names(training))],method="pca",thres=.9)