#################################
# utilityBelt.R
# written by Jet Goodson 
# started on 8 February 2013
# contact at jetgoodson@gmail.com
#
# general file to hold utility functions
# for character recognition, use other
# functions/macros to call
##################################

#gets a SparseM sparse matrix from a data frame
getSparse <- function(dataFrame) {
    tmpMat <- as.matrix(dataFrame)
    sparseMat <- as.matrix.csr(tmpMat)
    rm(tmpMat)
     return(sparseMat)
} #end of getSparse

#splits dataset into train and test subcomponents
culler <- function(dataset, trainPercent) {

 #split data set for cross validation by sampling from indices
   indices  <- 1:nrow(dataset)
   splitdex <- sample(indices, trunc(length(indices)*(100 - trainPercent)/100))
   subTest  <- dataset[splitdex,]
   subTrain <- dataset[-splitdex, ]

   cat(c("Test is ",  nrow(subTest),  " rows\n"))
   cat(c("Train is ", nrow(subTrain), " rows\n"))

   return( list(subTrain, subTest) )

} #end of culler


#this takes a matrix representing an image and reduces it's resolution by
#the resFactor, i.e. resFactor = 1/2, 2x2 squares are averaged, 1/4 -> 4x4
deRez <- function(theMatrix, resFactor) {
   if(resFactor == 1) {
      return(theMatrix)
   }

   resFactor <- 1/resFactor #feed it what you want the res reduced by (i.e., 1/2 the x pixels, means 2x2 merge)

   theWomb <- matrix(0, nrow = nrow(theMatrix)/resFactor, ncol = ncol(theMatrix)/resFactor) #new char image matrix 
   for(i in 1:nrow(theWomb)) {
      for(j in 1:ncol(theWomb)) {
         newRindex <- c((resFactor*i - resFactor + 1):(resFactor*i)) #figuring out which pixels get merged
         newCindex <- c((resFactor*j - resFactor + 1):(resFactor*j))

         for(x in newRindex){
            for(y in newCindex){
               theWomb[i,j] <- theWomb[i,j] + theMatrix[x,y]/(resFactor*resFactor) 
   }}}} #end of all the loops
   return(theWomb)
} #end of deRez


#this applies deRez to each row of the data frame
deRezDataFrame <- function(frameJob, resFactor, hasLabels) {
   
   #deal with difference between test and train
   adjust <- 0
   if(hasLabels == TRUE) {
       adjust <- 1
   }
   dimension <- sqrt(ncol(frameJob) - adjust) #assume square matrix
   newColSize = dimension*dimension*resFactor*resFactor + adjust #new number of columns in data frame
   newFrame <- data.frame(matrix(0, ncol = newColSize, nrow = nrow(frameJob)))
  
   for(z in 1:nrow(frameJob)) {
      if((z %% 1000) == 0) {
         cat(c(z, "\n"))
      }

      imatrix <- matrix(0, nrow = 2, ncol = 2) #matrix to hold image
      #test vs train
      if(adjust == 0) {
         imatrix <- matrix(frameJob[z, ], ncol = dimension)      
      }
      if(adjust == 1) {
         imatrix <- matrix(frameJob[z, -1], ncol = dimension)      
      }
            
      imatrix <- as.character(imatrix)
      imatrix <- as.numeric(imatrix) #this seems necessary to use make numeric
      imatrix <- matrix(imatrix, ncol = dimension)
      
      imatrix <- deRez(imatrix, resFactor)

      newRow <- c(as.vector(imatrix))
      if(adjust == 1) {  #add labels back in
         newRow <- c(frameJob[z, 1], newRow)
      }

      newFrame[z, ] <- newRow
   } #end of the loop

   return(newFrame)
} #end of deRezDataFrame function



#function to write matrix to new csv
writeDeRezedDataFrame <- function(inputName, outputName, resFactor, hasLabels = TRUE) {
  
   cat(c("Reading in ", inputName, "\n"))

   inputFrame <- read.csv(inputName, header=FALSE,skip=1,stringsAsFactors=FALSE)
 
   cat(c("Read in ", inputName, "\n"))

   outputFrame <- deRezDataFrame(inputFrame, resFactor, hasLabels)

   write.csv(outputFrame, file = outputName, row.names=FALSE)
   cat(c("Wrote data frame to csv file", outputName, "\n\n"))
 
   return()
} #end of writeDeRezedDataFrame function



#this applies a jitter given by the x and y variables. for a matrix
jitterBug <- function(theMatrix, x, y){

     library(matrixcalc)

     if(x > 0) {
          theMatrix <- shift.right(theMatrix, x)
     }
     if(x < 0) {
          theMatrix <- shift.left(theMatrix, abs(x))
     }

     if(y > 0) {
          theMatrix <- shift.up(theMatrix, y)
     }
     if(y < 0) {
          theMatrix <- shift.down(theMatrix, abs(y))
     }

     return(theMatrix)
} #end of jitterbug

#this applies the jitter to the whole dataset at once (more efficient, faster)
jitterHive <- function(dataset, x, y) {
  dimension <- sqrt(ncol(dataset) - 1)
  if(y > 0) {
    dataset <- cbind(dataset[,1],
                     dataset[,(2 + y*dimension):(dimension*dimension + 1)],
                     matrix(0, nrow=nrow(dataset), ncol=(y*dimension)))
  }#y>0
  if(y < 0) {
    colKill = dimension*dimension + 1 + y*dimension
    dataset <- cbind(dataset[,1],
                     matrix(0, nrow=nrow(dataset), ncol=abs(y)*dimension),
                     dataset[,2:colKill])
  }#y<0
 
  if(x > 0) {
    for(i in 1:x){
      dataset <- cbind(dataset[,1],
                      matrix(0, nrow=nrow(dataset), ncol=1),
                      dataset[,2:(ncol(dataset)-1)])
      for(j in 1:(dimension-1)){
        dataset[,(2+j*dimension)] <- matrix(0, nrow=nrow(dataset), ncol=1)
      }
    }
  } #x>0
  if(x < 0) {
    for(i in 1:abs(x)){
      dataset <- cbind(dataset[, 1],
                       dataset[, 3:(dimension*dimension + 1)],
                       matrix(0, nrow=nrow(dataset), ncol=1))
      for(j in 1:(dimension-1)){
        dataset[,(dimension*dimension + 1 - j*dimension)] <- matrix(0, nrow=nrow(dataset), ncol=1)
      }
    }
  }#x<0
            
  return(dataset)
} #end of jitterhive  


#create a synthetic dataset using jitterBox with a jitter in any direction given by jitterSize box (0,0) excluded
synthesizeDataSetFast <- function(dataset, jitterSize) {
  dataset     <- as.matrix(dataset)
  datasetOrig <- as.matrix(dataset)
  for(x in -jitterSize:jitterSize) {
    for(y in -jitterSize:jitterSize) {
      if(!(x == 0 && y == 0)) {
        dataset <- rbind(dataset, jitterHive(datasetOrig, x, y))
      }}}
  return(getSparse(dataset))
}#end of synthesizeDataSetFast


#create a synthetic dataset using jitterBox with a jitter in any direction given by jitterSize box (0,0) excluded
synthesizeDataSet <- function(dataset, jitterSize) {
    
    dataset <- as.matrix(dataset)
    dimension <- sqrt(ncol(dataset) - 1)
   
    for(z in 1:nrow(dataset)) {

         if((z %% 50) == 0) {
            cat(c(z, "\n"))
         }

         imatrix <- matrix(dataset[z, -1], ncol = dimension)   
         imatrix <- as.character(imatrix)
         imatrix <- as.numeric(imatrix) #this seems necessary to use make numeric
         imatrix <- matrix(imatrix, ncol = dimension)
   
         #image(imatrix)
         #dev.copy(png, paste(c("testImages/matrix", z, "shiftx", "n", "0", "shifty", "n", "0", "label", dataset[z,1], "png"), collapse="."))
         #dev.off()

         for(x in -jitterSize:jitterSize) {
          for(y in -jitterSize:jitterSize) {
            if(!(x == 0 && y == 0)) {
            newMatrix <- jitterBug(imatrix, x, y)
            newRow <- c(as.vector(newMatrix))
            newRow <- c(dataset[z, 1], newRow)
            dataset <- rbind(dataset, newRow)
           }
          }#end of y loop
        }#end of x loop
    }#end of row loop

   return(getSparse(dataset))

} #end of virtualDataSet




#look at distributions for numbers for new features
featureExplore <- function(dataFrame, label) {

  library(Matrix)
  
  system("mkdir -p plots")

  dataFrame <- dataFrame[dataFrame$V1 == label, -1]
  dimension <- sqrt(ncol(dataFrame))
   
  vecSimpleFraction  <- vector()
  vecFraction        <- vector()
  vecMeanX           <- vector()
  vecMeanY           <- vector()
  vecUpLeft          <- vector()
  vecUpRight         <- vector()
  vecDownRight       <- vector()
  vecDownLeft        <- vector()
 
  
  for(z in 1:nrow(dataFrame)) {
    
    if((z %% 500) == 0) {
      cat(c(z, "\n"))
    }

    imatrix <- matrix(dataFrame[z, ], ncol = dimension)   
    imatrix <- as.character(imatrix)
    imatrix <- as.numeric(imatrix) #this seems necessary to use make numeric
    imatrix <- matrix(imatrix, ncol = dimension)
   
    vecSimpleFraction <- c(vecSimpleFraction, nnzero(imatrix)/(dimension*dimension))
    vecFraction <- c(vecFraction, sum(sum(imatrix))/(dimension*dimension))

    tots <- sum(sum(imatrix))
    
    values <- c(1:dimension)
    sumOfRows <- rowSums(imatrix)    
    sumOfColumns <- colSums(imatrix)
    meanX <- sum(sumOfColumns * values)/tots
    meanY <- sum(sumOfRows * values)/tots
    vecMeanX <- c(vecMeanX, meanX)
    vecMeanY <- c(vecMeanY, meanY)
    meanX <- as.integer(meanX) 
    meanY <- as.integer(meanY)
    #cat(c(label, ": ", meanX, ", ", meanY, "\n"))
    vecUpLeft <- c(vecUpLeft, sum(sum(imatrix[(meanY+1):dimension, 1:meanX]))/tots)
    vecUpRight <- c(vecUpRight, sum(sum(imatrix[(meanY+1):dimension, (meanX+1):dimension]))/tots)
    vecDownLeft <- c(vecDownLeft, sum(sum(imatrix[1:meanY, 1:meanX]))/tots)
    vecDownRight <- c(vecDownRight, sum(sum(imatrix[1:meanY, (meanX+1):dimension]))/tots) 
  }#end of loop

  hist(vecSimpleFraction, breaks=(0:100)/100, xlab="Fraction of Cells Filled", ylab = "Count/0.01", main="Fraction of Cells Which Have Non-Zero Value")
  dev.copy(png, paste(c("plots/hist_simpleFraction", label, "png"), collapse="."))
  dev.off()
   
  hist(vecFraction, breaks=(0:100)/100, xlab="Fraction Filled", ylab = "Count/0.01", main="Fraction of Filled Space")
  dev.copy(png, paste(c("plots/hist_fraction", label, "png"), collapse="."))
  dev.off()

  hist(vecMeanX, breaks=dimension*(30:70)/100, xlab="X-Mean", ylab = "Count/0.01", main="X-Mean")
  dev.copy(png, paste(c("plots/hist_meanX", label, "png"), collapse="."))
  dev.off()

  hist(vecMeanY, breaks=dimension*(30:70)/100, xlab="Y-Mean", ylab = "Count/0.01", main="Y-Mean")
  dev.copy(png, paste(c("plots/hist_meanY", label, "png"), collapse="."))
  dev.off()


  hist(vecUpLeft, breaks=(0:100)/100, xlab="Fraction", ylab = "Count/0.01", main="Fraction in Upper Left")
  dev.copy(png, paste(c("plots/hist_upLeft", label, "png"), collapse="."))
  dev.off()

  hist(vecUpRight, breaks=(0:100)/100, xlab="Fraction", ylab = "Count/0.01", main="Fraction in Upper Right")
  dev.copy(png, paste(c("plots/hist_upRight", label, "png"), collapse="."))
  dev.off()

  

  hist(vecDownLeft, breaks=(0:100)/100, xlab="Fraction", ylab = "Count/0.01", main="Fraction in Lower Left")
  dev.copy(png, paste(c("plots/hist_downLeft", label, "png"), collapse="."))
  dev.off()

  hist(vecDownRight, breaks=(0:100)/100, xlab="Fraction", ylab = "Count/0.01", main="Fraction in Lower Right")
  dev.copy(png, paste(c("plots/hist_downRight", label, "png"), collapse="."))
  dev.off()
 
}#end of feature explore
