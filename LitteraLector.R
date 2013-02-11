#################################
# LitteraLector.R
# written by Jet Goodson 
# started in February 2009
# contact at jetgoodson@gmail.com
#
#first     > source("LitteraLector.R")
#then call > LitteraLector()
###########################################

LitteraLector <-function() {

   library(e1071)                                  #get the SVM tools
   library(caret)                                  #get the zeroVar tools
   source("utilityBelt.R")                         #custom utility tools
   source("rageAgainstTheSupportVectorMachine.R")  #our calls to SVM

   deRezData <- FALSE  #this will reduce the resolution of the original data
   deRezFactor <- 0.5  #a factor of 1/2 means that a 2x2 cluster of pixels becomes 1 pixel
                    #and a four fold decrease in features

   testInput  = "data/test_ReducedRez.csv" #input files
   trainInput = "data/train_ReducedRez.csv"



   if(deRezData == TRUE) {
      preTest = "data/test.csv"
      preTrain = "data/train.csv"

      constantColumns <- writeDeRezedDataFrame(preTrain, testInput, deRezFactor)
      constantColumns <- constantColumns - 1 #test data doesn't have labels, have to shift list down
      constantColumns <- writeDeRezedDataFrame(preTest, trainInput, deRezFactor, constantColumns)
   }


   trainFrame <- read.csv(trainInput, header=FALSE,skip=1,stringsAsFactors=FALSE)
   testFrame  <- read.csv(testInput,  header=FALSE,skip=1,stringsAsFactors=FALSE)
  
   svmModel <- rageAgainstTheSupportVectorMachine(trainFrame)


   return()

} #end of LitteraLector macro
