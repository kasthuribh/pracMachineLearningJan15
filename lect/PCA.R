#PCA
set.seed(32333)

library(caret); library(kernlab);data(spam)
inTrain<-createDataPartition(y = spam$type,p = 0.75, list = FALSE)
trainSet<-spam[inTrain,]
testSet<-spam[-inTrain,]

M <- abs(cor(trainSet[,-58]))
diag(M) <- 0 #set the correlation between the same relation to 0
which(M > 0.8, arr.ind = T)

#examines highly correlated relationships
names(spam)[c(32,31)]
plot(spam[,32], spam[,31])

names(spam)[c(32,34)]
plot(spam[,32], spam[,34])

#basic PCA ideas

#combining variables to capture "most information" possible

#basicall sum and subtracting two variables
X<-0.71*trainSet$num415 + 0.71 * trainSet$num857
Y<-0.71*trainSet$num415 - 0.71 * trainSet$num857
plot(X,Y)
#0.71 rotates the graph
#now most of the variability is happening at the X axis
# most of the variability is happening at the X axis,
#However most of the variables are not spreaded along y
# thus in here adding captures "most information",
# but subtraction takes less information 

#principal components in R
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
#principal component 1 vs principal component 2
plot(prComp$x[,1], prComp$x[,2])
# similar to addition and subtraction similar to earlier

prComp$rotation
#PC1 (addition) explains the "most variability" in this particular case

#PCA on spam data
typeColor <- ((spam$type=="spam")*1+1) #color black if spam, red if ham
# to make the data more gaussian looking
# for PCA we do it most of the time
prComp<-prcomp(log10(spam[,-58]+1)) 
plot(prComp$x[,1],prComp$x[,2], col=typeColor, xlab = "PC1", ylab = "PC2")

