#################################
# LitteraLector.R
# written by Jet Goodson and John Backus Meyer
# started in February 2013
# contact at jetgoodson@gmail.com
#
#first     > source("LitteraLector.R")
#then call > LitteraLector()
###########################################
#!/usr/bin/env Rscript 
LitteraLector <-function() {

   library(e1071)                                  #get the SVM tools
   #library(caret)                                  #get the zeroVar tools
   library(SparseM)                                  #for sparse matrices
   source("utilityBelt.R")                         #custom utility tools
   source("rageAgainstTheSupportVectorMachine.R")  #our calls to SVM
   source("visualyze.R")                           # graphical tools for output

   outputLog <- "logFile.dat"
   sink(outputLog, append=TRUE, split=TRUE)
   
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
   bestCoef  <- 0.5

   loadSavedSVM  <- FALSE
   doTune        <- FALSE
   tuneFraction  <- 10
   trainFraction <- 80

   parallelNodes <- 2 #2 cores on my laptop

   ##################################################################################  End of config

  

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
   trainFrame[,-1] <- trainFrame[,-1]/255.0;
   trainSparse <- getSparse(trainFrame)
   rm(trainFrame)
   
   testFrame  <- read.csv(testInput,  header=FALSE,skip=1,stringsAsFactors=FALSE)
   testFrame <- testFrame/255.0;
   testSparse <- getSparse(testFrame)
   rm(testFrame)

   cat("Loaded the training and test data\n")

   if(loadSavedSVM == FALSE) {
       trainAndTest <- culler(trainSparse, trainFraction)
       #trainAndTest <- c(trainSparse, trainSparse)
       
       if(doTune == TRUE) {
          #tuned <- tuneMachine(trainAndTest[[1]], tuneFraction, gams = bestGamma, costs = 10^(-1:1))
          #bestGamma <- tuned$best.parameters[[1]] #get best gamma
          #bestCost  <- tuned$best.parameters[[2]] #get best cost
          #cat(c("Tuned that bad bow up\n"))
         results <- parallelTuneMachine(trainAndTest[[1]], tuneFraction, parallelNodes)
         bestCost <- results[1]
         poly <- results[2]
         bestGamma <- results[3]
         bestCoef <- results[4]
       }
      svmModel <- rageAgainstTheSupportVectorMachine(trainAndTest[[1]], svmModelName, poly, bestGamma, bestCost, bestCoef) ##double [[n]] needed for list elements
      dataSave <- list(trainAndTest, svmModel)  #put them in list to save list
      save(dataSave, file=paste(c(svmModelName, "rda"), collapse="."))
   }
   if(loadSavedSVM == TRUE){
      print(load(file=paste(c(svmModelName, "rda"), collapse=".")))
      svmModel <- dataSave[[2]]
      print(summary(svmModel))
      trainAndTest <- dataSave[[1]]
   }

   #get a data frame with the indices, label, and prediction of images which where erroniously  classified 
   failedFrame <- validateMachine(svmModel, trainAndTest[[2]], svmModelName)
   failedFrame <- validateMachine(svmModel, trainSparse, paste(svmModelName,"Full",sep=""))
   svmPred <- applyMachine(svmModel, testSparse, svmModelName)

   
   supportVectors <-  trainAndTest[[1]][svmModel$index, ]
   cat(c("Number of SVs: ", nrow(supportVectors), "\n"))
   virtualTrainSet <- synthesizeDataSetFast(supportVectors, 2)  #use translational jitter to synthesize data from the support vectors
   cat(c("Resulting number: ", nrow(virtualTrainSet), "\n"))
   
   virtualSVM <- rageAgainstTheSupportVectorMachine(virtualTrainSet, virtualSVMModelName, poly, bestGamma, bestCost, bestCoef)
   virtualFailedFrame <- validateMachine(virtualSVM, trainAndTest[[2]], virtualSVMModelName)
   virtualFailedFrame <- validateMachine(virtualSVM, trainSparse, paste(virtualSVMModelName,"FULL",sep=""))
   applyMachine(virtualSVM, testSparse, virtualSVMModelName)
   
   

   #synthSVMModelName <- "synthSVMTest"
   #synthTrainSet     <- synthesizeDataSet(trainAndTest[[1]], 1)  #use translational jitter to synthesize data from the support vectors
   #synthSVM          <- rageAgainstTheSupportVectorMachine(synthTrainSet, synthSVMModelName, poly, bestGamma, bestCost)
   #synthFailedFrame  <- validateMachine(synthSVM, trainAndTest[[2]], synthSVMModelName)




   return("Vis vobiscum")

} #end of LitteraLector macro
