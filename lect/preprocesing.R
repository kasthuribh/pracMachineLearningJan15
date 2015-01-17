#preprocessing
set.seed(32333)

library(caret); library(kernlab);data(spam)
inTrain<-createDataPartition(y = spam$type,p = 0.75, list = FALSE)
trainSet<-spam[inTrain,]
testSet<-spam[-inTrain,]

hist(trainSet$capitalAve, main="", xlab="ave. capital run length")
#very skewed distribution
#so you might want to preprocess
mean(trainSet$capitalAve)
sd(trainSet$capitalAve)
#huge SD

#standardize variables
#usual way, (val-mean)/sd
#known as centering and scaling
trainCapAve <- trainSet$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

hist(trainCapAveS, main="", xlab="ave. capital run length")

#standardizing TEST SET
#important, when we apply this to test set we need to
#apply mean and SD of the TRAIN SET

testCapAve <- testSet$capitalAve
testCapAveS<-(testCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

#standardizing - preProcess function

preObj <- preProcess(trainSet[,-58], method=c("center","scale"))
trainCapAveS <- predict(preObj, trainSet[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

#same preObj earlier can be used to standardize the test set
testCapAveS <- predict(preObj, testSet[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

#standardizing - preProcess argument
set.seed(32343)
modelFit <- train(type~., data=trainSet, preProcess = c("center","scale"), method="glm")
modelFit

#standarding Box-Cox tranforms
#takes set of continious data and try to make the look like
#normally distributed data using maximum likelyhood

preObj <- preProcess(trainSet[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, trainSet[,-58])$capitalAve
par(mfrow=c(1,2));hist(trainCapAveS); qqnorm(trainCapAveS)

#standardizing - Imputing data
#missing data

set.seed(13343)

#Make some values NA
trainSet$capAve <- trainSet$capitalAve #new column
#generate random values
selectNA <- rbinom(dim(trainSet)[1],size=1,prob=0.05)==1
#set those values to missing
trainSet$capAve[selectNA]<- NA

#Impute and standardize
#apply k-nearest neighbour imputation for missing values
preObj <- preProcess(trainSet[,-58],method="knnImpute")
capAve <- predict(preObj, trainSet[,-58])$capAve

#standardize true values without missing values
capAveTruth <- trainSet$capitalAve
capAveTruth <- (capAveTruth)-mean(capAveTruth)/sd(capAveTruth)

#compare actual and imputed
quantile(capAve-capAveTruth)

#compare only missing values
quantile((capAve-capAveTruth)[selectNA])






