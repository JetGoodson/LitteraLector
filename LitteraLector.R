#################################
# LitteraLector.R
# written by Jet Goodson and John Backus Meyer
# started in February 2009
# contact at jetgoodson@gmail.com
#
#first     > source("LitteraLector.R")
#then call > LitteraLector()
###########################################

LitteraLector <-function() {

   library(e1071)                                  #get the SVM tools
   #library(caret)                                  #get the zeroVar tools
   source("utilityBelt.R")                         #custom utility tools
   source("rageAgainstTheSupportVectorMachine.R")  #our calls to SVM
   source("visualyze.R")                           # graphical tools for output

   deRezData <- FALSE  #this will reduce the resolution of the original data
   deRezFactor <- 0.5  #a factor of 1/2 means that a 2x2 cluster of pixels becomes 1 pixel
                    #and a four fold decrease in features

   testInput  <- "data/test_ReducedRez.csv" #input files
   trainInput <- "data/train_ReducedRez.csv"

   svmModelName <- "svmTest"

   loadSavedSVM <- TRUE

   if(deRezData == TRUE) {
      preTest = "data/test.csv"
      preTrain = "data/train.csv"

      writeDeRezedDataFrame(preTrain, trainInput, deRezFactor)
      writeDeRezedDataFrame(preTest, testInput, deRezFactor, hasLabels = FALSE)
   }

   trainFrame <- read.csv(trainInput, header=FALSE,skip=1,stringsAsFactors=FALSE)
   testFrame  <- read.csv(testInput,  header=FALSE,skip=1,stringsAsFactors=FALSE)

   cat("Loaded the training and test data\n")


   #get rid of constant columns
   constantColumns <- sapply(trainFrame, function(column){ all(column[1] == column) })
   constantColumns <- as.vector(constantColumns)
   constantColumns <- which(constantColumns)
   trainFrame <- trainFrame[, -constantColumns]
   constantColumns <- constantColumns - 1 #test data doesn't have labels, have to shift list down
   testFrame <- testFrame[, -constantColumns]  

   cat("Nixed the constant features\n")

   if(loadSavedSVM == FALSE) {
      trainAndTest <- culler(trainFrame, 2)
      svmModel <- rageAgainstTheSupportVectorMachine(trainAndTest[[1]], svmModelName) ##double [[n]] needed for list elements
      dataSave <- list(trainAndTest, svmModel)
      save(dataSave, file=paste(c(svmModelName, "rda"), collapse="."))
   } #create the SVM
   if(loadSavedSVM == TRUE){
      print(load(file=paste(c(svmModelName, "rda"), collapse=".")))
      svmModel <- dataSave[[2]]
      trainAndTest <- dataSave[[1]]
   }


   failedPredictices <- validateMachine(svmModel, trainAndTest[[2]], svmModelName)


   return("Vis vobiscum")

} #end of LitteraLector macro
