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
qq<-qplot(y = wage, x=age, data = trainSet, color = education)
qq + geom_smooth(method = "lm", formula = y~x)

#cut2 , making factors (Hmisc package)
library(Hmisc)
#cut the data set in to three group based on wage
cutWage <- cut2(trainSet$wage, g=3)
table(cutWage)

#different wage groups vs age
p1 <- qplot (cutWage, age, data=trainSet, fill=cutWage, geom=c('boxplot'))
p1

#box plot with points overlayed
p2 <- qplot(cutWage, age, data=trainSet, fill=cutWage, geom=c("boxplot","jitter"))
#grid arrange from gridExtra makes two plots side by side
library(gridExtra)
grid.arrange(p1,p2,ncol=2)

#tables
t1 <- table(cutWage, trainSet$jobclass)
t1

#propotion tables
prop.table(t1,1)

#density plot
qplot(wage, colour=education, data=trainSet, geom="density")
