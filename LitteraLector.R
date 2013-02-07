LitteraLector <-function() {


library(e1071)

  
trainData <- read.csv("data/train.csv", colClasses = "character")

trainData <- trainData[-1,] #get rid of headers


#the e1071 can handle a frame, but it seems easier just to split these up into x and y arguments

images <- subset(trainData, select = -c(label) )  #get rid of labels

labels <- trainData["label"] #get only the labels



index <- 1:nrow(images)
testindex <- sample(index, trunc(length(index)*30/100))

testImages <- images[testindex,]
trainImages <- images[-testindex,]

testLabels <- labels[testindex,]
trainLabels <- labels[-testindex,]



index <- 1:nrow(trainData)

testindex <- sample(index, trunc(length(index)*99/100))

testset <- trainData[testindex,]
trainset <- trainData[-testindex,]


trainset$label[trainset$label!=1] <- 0


tuned <- tune.svm(pixel0~., data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1))






tuned <- tune.svm(x = trainImages, y = trainLabels, gamma = 10^(-6:-1), cost = 10^(-1:1))



#model <- svm(x = images, y = labels, 




library(e1071)
library(caret)
 

  
trainData <- read.csv("data/train.csv",header=FALSE,skip=1)

# get indices of data.frame columns (pixels) with low variance
badCols <- nearZeroVar(trainData)
trainData <- trainData[, -badCols]


index <- 1:nrow(trainData)

testindex <- sample(index, trunc(length(index)*90/100))

testset <- trainData[testindex,]
trainset <- trainData[-testindex,]


trainset$V1[trainset$label!=1] <- 0


tuned <- tune.svm(V153~., data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1))





} #end of R macro
