#################################
# rageAgainstTheSupportVectorMachine.R
# written by Katie Malone & Jet Goodson 
# started on 8 February 2009
# contact at cmmalone.158@gmail.com, jetgoodson@gmail.com
#
# functions to do SVM
##################################


rageAgainstTheSupportVectorMachine <-function(dataset, modelName, poly, bestGamma, bestCost) {


    responseVector <- as.vector(as.matrix(dataset[,1]))
    library(e1071)  #get the e1071 library, which accesses libsvm
model <- svm(x = dataset[,-1], y = responseVector, scale=FALSE, type = "C-classification",  kernel="polynomial", degree = poly, coef0 = 0.5, gamma = bestGamma, cost = bestCost, cross = 10, probability = TRUE)
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
tuneMachine <-function(dataset, tunePercent) {

  #first tune, the model using a grid search over gamma and cost to find
   #the best results
   #V1 should be the labeled column, I dunno if that is a stable name though
   #I've also seen it get called X1 --- I'm hoping that was because it was
   #a derived data frame

    indices     <- 1:nrow(dataset)
    splitdex <- sample(indices, trunc(length(indices)*(tunePercent)/100))
    subTune  <- dataset[splitdex,]
   
    tuned <- tune.svm(V1~., data = dataset, kernel="polynomial", gamma = 10^(-6:-1), cost = 10^(-1:1))
    print(summary(tuned))
}#end of tuneMachine



 
#virtualRage











































