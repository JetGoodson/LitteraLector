LitteraLector <-function() {

trainData <- read.csv("data/train.csv", colClasses = "character")

trainData <- trainData[-1,] #get rid of headers


#the e1071 can handle a frame, but it seems easier just to split these up into x and y arguments

images <- subset(trainData, select = -c(label) )  #get rid of labels

labels <- trainData["label"] #get only the labels

}
