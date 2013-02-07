rageAgainstTheSupportVectorMachine <-function() {


library(e1071)
library(caret)
 
trainData <- read.csv("data/train.csv",header=FALSE,skip=1)

constantCols <- nearZeroVar(trainData)
trainData <- trainData[, -constantCols]

index <- 1:nrow(trainData)
testindex <- sample(index, trunc(length(index)*99/100))
testset <- trainData[testindex,]
trainset <- trainData[-testindex,]
 
trainset$V1[trainset$label!=1] <- 0



tuned <- tune.svm(V153~., data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1))

#at this point we know the cost and gamma to use...this needs replicated though
#still have to train...
#and this was 1% of the training set. Doing 30% was baking my shitty laptop

} #end of R macro
