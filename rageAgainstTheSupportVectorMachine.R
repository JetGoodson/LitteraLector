# at the beginning of a work session, call this like so:
# trainData = supportTheSupport() to read in the csv file only once
# (saves some time in reading)

supportTheSupport = function() {

    trainData <- read.csv("data/train.csv",header=FALSE,skip=1)
    return(trainData)
}

rageAgainstTheSupportVectorMachine <-function(trainData) {


    library(e1071)
    library(caret)
 

    constantCols <- nearZeroVar(trainData)
    trainData <- trainData[, -constantCols]

    index <- 1:nrow(trainData)
    testindex <- sample(index, trunc(length(index)*99/100))
    testset <- trainData[testindex,]
    trainset <- trainData[-testindex,]
    cat("info: the size of the train set is ", nrow(trainset), "\n")
    cat("info: the size of the test set is ", nrow(testset), "\n")
 
    trainset$V1[trainset$label!=1] <- 0


    tuned <- tune.svm(V153~., data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1))
#    tuned$cost
#    tuned$gamma
#    cat("info: after tuning, gamma is ", gamma, "\n")
#    cat("info: after tuning, cost is ", cost, "\n")

    #at this point we know the cost and gamma to use...this needs replicated though
    #still have to train...
    #and this was 1% of the training set. Doing 30% was baking my shitty laptop

} #end of R macro
