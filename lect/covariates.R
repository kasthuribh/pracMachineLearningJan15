#covariate craetion = feature creation
library(ISLR); library(caret); data(Wage);
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

#common covariates to add, dummy variables
#Basic idea - convert factor variables to indicator variables
#             convert qualitative or factor to dummy variables
table(training$jobclass)

#convert jobclass to dummy variables
dummies <- dummyVars( wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

#removing zero covariates
#variables with very low variability
nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv
# sex and reigion are less meaningful to have

#Spline basis
#fitting a curvy line
library(splines)
# bs function will create a polynomial variable
# df=3 three degree polynomial
bsBasis <- bs(training$age, df=3)
#column 1 -> age values, scaled for computational purposes
# column 2 -> similar to age squared (square relationship between age and the outcome)
# column 3 ->
bsBasis
#Fitteing curves with splines
lm1 <- lm(wage~bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col='red', pch=19, cex=0.5)

#same bsBasis should be used with the TEST SET as well
predict(bsBasis, age=testing$age)
