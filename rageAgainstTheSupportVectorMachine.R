#################################
# rageAgainstTheSupportVectorMachine.R
# written by Katie Malone & Jet Goodson 
# started on 8 February 2009
# contact at cmmalone.158@gmail.com, jetgoodson@gmail.com
#
# functions to do SVM
##################################


rageAgainstTheSupportVectorMachine <-function(dataset, modelName) {


   library(e1071)  #get the e1071 library, which accesses libsvm


  # dataset$V1 <- as.character(dataset$V1)#did not work

   #split data set for cross validation by sampling from indices
   indices  <- 1:nrow(dataset)
   splitdex <- sample(indices, trunc(length(indices)*98/100))
   subTest  <- dataset[splitdex,]
   subTrain <- dataset[-splitdex, ]

   cat(c("Test is ",  nrow(subTest),  " rows\n"))
   cat(c("Train is ", nrow(subTrain), " rows\n"))


   #first tune, the model using a grid search over gamma and cost to find
   #the best results
   #V1 should be the labeled column, I dunno if that is a stable name though
   #I've also seen it get called X1 --- I'm hoping that was because it was
   #a derived data frame

   tuned <- tune.svm(V1~., data = subTrain, kernel="polynomial", gamma = 10^(-6:-1), cost = 10^(-1:1))
   
   #C-classification is the default
   #the polynomial goes as (X + C)^p
   # default C is 0, default p is 3
   #gamma indicates the range of gammas to try, 10^-6, 10^-5, ...10^1
   #same with cost, 0.1, 1, 10

   print(summary(tuned))

   bestGamma <- tuned$best.parameters[[1]] #get best gamma
   bestCost  <- tuned$best.parameters[[2]] #get best cost

    model <- svm(V1~., data = subTrain, type = "C-classification", kernel="polynomial", gamma = bestGamma, cost = bestCost, cross = 10, probability = FALSE)
    #most of these are the same as for tune, but this actually
    #builds the SVM with the gamma and cost we feed in
    #cross is some sort of cross validation checking

    #by default it does 1-vs-1 on multiple levels, so it really does
    #k*(k-1)/2 SVMs, so a 1 vs 9, a 2 vs 7, etc, and then just votes
    #I don't really understand the output though, it isn't just giving me 
    #a predicted digit

   print(summary(model))

   prediction <- predict(model, subTest, decision.values = TRUE, probability = FALSE)
   #this actually applies the model to the test sub-dataset

   #tab <- table(pred = prediction, true = subTest$V1)
   tab <- table(prediction, subTest$V1)   
   #this produces a table of prediction versus actually, or it should
   #I don't really understand the output I get from the 1-vs-1 model

#   drawPredictionTruthTable(tab)   # just prints the table 
   drawPerformanceMatrix(tab)#, paste(c("prediction_table", modelName, "png"), collapse="."))
   write.table(tab, file=paste(c("prediction_table", modelName, "txt"), collapse="."))


   return(model)

} #end of rageAgainstTheSupportVectorMachine


#will add function to do prediction on actual test dataset



