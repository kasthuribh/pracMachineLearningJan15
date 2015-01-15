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