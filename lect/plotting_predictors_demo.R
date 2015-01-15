#week 2 lect 4
library(ISLR);library(ggplot2);library(caret)
data(Wage)
summary(Wage)

inTrain<-createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
trainSet<-Wage[inTrain,]
testSet<-Wage[-inTrain,]
dim(trainSet); dim(testSet)

featurePlot(y = trainSet$wage, x = trainSet[,c("age", "education","jobclass")], plot = "pairs")

#age vs Wage
qplot(y = wage, x=age, data = trainSet)

#qplot with coloured by job class
qplot(y = wage, x=age, data = trainSet, color = jobclass)

#add regression smoothers
