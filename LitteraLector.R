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

   fullFeatured <- FALSE  #whether to use all 800 features or to use the 200 feature data
   testInput  <- "data/test_ReducedRez.csv" #input files
   trainInput <- "data/train_ReducedRez.csv"

   svmModelName        <- "svmTest"
   virtualSVMModelName <- "virtualSVMTest"
   poly = 3  #  a degree of 7 seems to be the max I can hack on a laptop, want 9

   bestGamma <- 0.5
   bestCost  <- 5 #these are taken from deCoste & Schoelkopf and used as defaults, but I switched cost to 5
 

   loadSavedSVM <- FALSE
   doTune       <- FALSE
   tuneFraction <- 100
   testFraction <- 1

   ##############################################################################################  End of config


   if(deRezData == TRUE) {
      preTest = "data/test.csv"
      preTrain = "data/train.csv"

      writeDeRezedDataFrame(preTrain, trainInput, deRezFactor)
      writeDeRezedDataFrame(preTest, testInput, deRezFactor, hasLabels = FALSE)
   }

   if(fullFeatured == TRUE) {
      testInput = "data/test.csv"
      trainInput = "data/train.csv"
   }

 
   trainFrame <- read.csv(trainInput, header=FALSE,skip=1,stringsAsFactors=FALSE)
   testFrame  <- read.csv(testInput,  header=FALSE,skip=1,stringsAsFactors=FALSE)

   cat("Loaded the training and test data\n")


   #get rid of constant columns
   constantColumns <- sapply(trainFrame, function(column){ all(column[1] == column) })
   constantColumns <- as.vector(constantColumns)
   constantColumns <- which(constantColumns)
   trainFrameNonConst <- trainFrame[, -constantColumns]
   adjConstantColumns <- constantColumns - 1 #test data doesn't have labels, have to shift list down
   testFrame <- testFrame[, -adjConstantColumns]  

   cat("Nixed the constant features\n")

    
   if(loadSavedSVM == FALSE) {
      trainAndTest <- culler(trainFrameNonConst, testFraction)

        if(doTune == TRUE) {
          tuned <- tuneMachine(trainedAndTest[[1]], tuneFraction)
          bestGamma <- tuned$best.parameters[[1]] #get best gamma
          bestCost  <- tuned$best.parameters[[2]] #get best cost
       }

      svmModel <- rageAgainstTheSupportVectorMachine(trainAndTest[[1]], svmModelName, poly, bestGamma, bestCost) ##double [[n]] needed for list elements
      dataSave <- list(trainAndTest, svmModel)  #put them in list to save list
      save(dataSave, file=paste(c(svmModelName, "rda"), collapse="."))
   } #create the SVM
   if(loadSavedSVM == TRUE){
      print(load(file=paste(c(svmModelName, "rda"), collapse=".")))
      svmModel <- dataSave[[2]]
      print(summary(svmModel))
      trainAndTest <- dataSave[[1]]
   }

   #get a data frame with the indices, label, and prediction of images which where erroniously  classified 
   failedFrame <- validateMachine(svmModel, trainAndTest[[2]], svmModelName)

   supportVectors <- getSupportVectors(svmModel, trainFrame) #get the actual support vectors
   cat(c("Number of SVs: ", nrow(supportVectors), "\n"))
   virtualTrainSet <- synthesizeDataSet(supportVectors, constantColumns, 1)  #use translational jitter to synthesize data from the support vectors
   cat(c("Resulting number: ", nrow(virtualTrainSet), "\n"))


 
   virtualSVM <- rageAgainstTheSupportVectorMachine(virtualTrainSet, virtualSVMModelName, poly, bestGamma, bestCost)
   virtualFailedFrame <- validateMachine(virtualSVM, trainAndTest[[2]], virtualSVMModelName)


   synthSVMModelName <- "synthSVMTest"
   synthTrainSet     <- synthesizeDataSet(trainAndTest[[1]], constantColumns, 1)  #use translational jitter to synthesize data from the support vectors
   synthSVM          <- rageAgainstTheSupportVectorMachine(synthTrainSet, synthSVMModelName, poly, bestGamma, bestCost)
   synthFailedFrame  <- validateMachine(synthSVM, trainAndTest[[2]], synthSVMModelName)




   return("Vis vobiscum")

} #end of LitteraLector macro
