#################################
# rageAgainstTheSupportVectorMachine.R
# written by Katie Malone & Jet Goodson 
# started on 8 February 2013
# contact at cmmalone.158@gmail.com, jetgoodson@gmail.com
#
# functions to do SVM
##################################


rageAgainstTheSupportVectorMachine <-function(dataset, modelName, poly, bestGamma, bestCost, bestCoef) {


    responseVector <- as.vector(as.matrix(dataset[,1]))
    library(e1071)  #get the e1071 library, which accesses libsvm
model <- svm(x = dataset[,-1], y = responseVector, scale=FALSE, type = "C-classification",  kernel="polynomial", degree = poly, coef0 = bestCoef, gamma = bestGamma, cost = bestCost, cross = 10, probability = TRUE)
    rm(responseVector)
    #model <- svm(V1~., data = dataset, type = "C-classification", kernel="polynomial", degree = poly, coef0 = 0.5, gamma = bestGamma, cost = bestCost, cross = 10, probability = TRUE)
    #most of these are the same as for tune, but this actually
    #builds the SVM with the gamma and cost we feed in
    #cross is some sort of cross validation checking

    #by default it does 1-vs-1 on multiple levels, so it really does
    #k*(k-1)/2 SVMs, so a 1 vs 9, a 2 vs 7, etc, and then just votes

    print(summary(model))
    return(model)

} #end of rageAgainstTheSupportVectorMachine, which is largely useless now


#this handles testing the machine on the training sub-test data and printed a graph and prediction matrix
validateMachine <-function(model, testSet, modelName) {

   testVector <- as.vector(as.matrix(testSet[,1]))

   prediction <- predict(model, testSet[,-1], decision.values = FALSE, probability = TRUE)
   #this actually applies the model to the test sub-dataset

   #this produces a table of prediction versus actually
   tab <- table(prediction, testVector)   
  
   accuracy <- 0
   tabMatrix <- data.matrix(tab)
   for(i in 1:nrow(tabMatrix)) {
        accuracy <- accuracy + tabMatrix[i, i]
    }
    accuracy <- 100.0*accuracy/nrow(testSet)
    cat(c(modelName, " validation accuracy is ", accuracy, "%\n"))
    cat("Sterling Archer voice: \"Which sucks, Lana. Do better.\" \n")

#   drawPredictionTruthTable(tab)   # just prints the table 
   drawPerformanceMatrix(tab, paste(c("prediction_table", modelName, "png"), collapse="."))
   write.table(tab, file=paste(c("prediction_table", modelName, "txt"), collapse="."))

   compareFrame <- as.data.frame(cbind(testVector,prediction))
   compareFrame <- compareFrame[which(prediction != testVector),]

   rm(testVector)
   return(compareFrame)
} #end of  validateMachine


#sets up model to be tuned in terms of cost and gamma (if needed)
tuneMachine <-function(dataset, tunePercent, gams, costs) {

  #first tune, the model using a grid search over gamma and cost to find
   #the best results
   #V1 should be the labeled column, I dunno if that is a stable name though
   #I've also seen it get called X1 --- I'm hoping that was because it was
   #a derived data frame

    indices     <- 1:nrow(dataset)
    splitdex <- sample(indices, trunc(length(indices)*(tunePercent)/100))
    subTune  <- dataset[splitdex,]

    cat("Tuning on ", nrow(subTune), " events\n")
    
    responseVector <- as.vector(as.matrix(dataset[,1]))
    tuned <- tune.svm(x = dataset[,-1], y = responseVector, kernel="polynomial", scale=FALSE, coef0 = 0.5, gamma = gams, cost = costs)
    rm(responseVector)
    print(summary(tuned))

    return(tuned)
}#end of tuneMachine



#this handles testing the machine on the training sub-test data and printed a graph and prediction matrix
applyMachine <-function(model, testSet, modelName) {

   prediction <- predict(model, testSet, decision.values = FALSE, probability = TRUE)
   #this actually applies the model to the test dataset
   
   write(as.vector(prediction), paste(c(modelName, "TestPrediction", "txt"), collapse="."), sep="\n")
   return(prediction) 
} #end of  validateMachine





#sets up model to be tuned parallel
parallelTuneMachine <-function(dataset, tunePercent, nodes,
                               costList = c(0.1, 0.5, 1, 2, 5, 10, 100),
                               gammaList = c(10^(-3:1), 5*10^(-1:1)),
                               coeffList = c(10^(-3:1), 5*10^(-1:1)),
                               polyList = c(2,3,4,5)) {
  library(foreach)
  library(doSNOW)
  library(e1071)  #get the e1071 library, which accesses libsvm
  cl <- makeCluster(nodes, type="SOCK") #2 nodes
  registerDoSNOW(cl)
  
  
  indices     <- 1:nrow(dataset)
  splitdex <- sample(indices, trunc(length(indices)*(tunePercent)/100))
  subTune  <- dataset[splitdex,]
  cat("Tuning on ", nrow(subTune), " events\n")

  responseVector <- as.vector(as.matrix(subTune[,1]))

  iterations <- (length(costList)*length(gammaList)*length(coeffList)*length(polyList))
  gridMatrix <- matrix(2, nrow=iterations, ncol=4);

  indo <- 1
  for(i in costList) {# cost loop
    for(j in gammaList) {# cost loop
      for(k in coeffList) {# cost loop
        for(l in polyList) {# cost loop
          gridMatrix[indo, 1] = i          
          gridMatrix[indo, 2] = j          
          gridMatrix[indo, 3] = k          
          gridMatrix[indo, 4] = l          
          indo <- indo + 1 
        }}}}  

  svmTune <- foreach(i = 1:iterations, .packages='e1071') %dopar% {
    svm(x = subTune[,-1], y = responseVector, scale=FALSE, type = "C-classification",  kernel="polynomial", degree = gridMatrix[i,4], coef0 = gridMatrix[i,3], gamma = gridMatrix[i,2], cost = gridMatrix[i,1], cross = 10, probability = FALSE)
  } #end of parallel loop
  stopCluster(cl)

  accuracy <- 0
  bestGamma <- 0
  bestCost <- 0
  bestPoly <- 0
  bestCoef <- 0
  
  for(model in svmTune){
    if(accuracy < mean(model$accuracies)) {
      bestGamma <- model$gamma
      bestCost <- model$cost
      bestPoly <- model$degree
      bestCoef <- model$coef0
      accuracy <- mean(model$accuracies) 
    }
  }
  
#cat(c("Best values: (", bestGamma, ", ", bestCost, ", ", bestCoef, ", ", bestPoly, ", gives ", accuracy, "% accuracy\n"))
  
 return(c(bestCost, bestPoly, bestGamma, bestCoef))
     
}#end of tuneMachine







































