#week 2 lect 1
library(caret); library(kernlab);data(spam)
inTrain<-createDataPartition(y = spam$type,p = 0.75, list = FALSE)
trainSet<-spam[inTrain,]
testSet<-spam[-inTrain,]
dim(trainSet)
set.seed(1234)
modelFit<-train(type~.,data = trainSet, method="glm")
modelFit
modelFit$finalModel
#spam[1,]
predictions<-predict(modelFit,newdata = testSet)
predictions
confusionMatrix(predictions,testSet$type)

#week 2 lect 2

#cross validation
set.seed(32323)
folds=createFolds(spam$type,k = 10, list = TRUE, returnTrain = TRUE)
sapply(folds,length)
folds[[1]][1:10]

set.seed(32323)
folds=createFolds(spam$type,k = 10, list = TRUE, returnTrain = FALSE)
sapply(folds,length)

set.seed(32323)
folds=createResample(y = spam$type, times = 10, list = TRUE)
sapply(folds,length)
folds[[1]][1:10]
