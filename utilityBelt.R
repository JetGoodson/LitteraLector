#################################
# utilityBelt.R
# written by Jet Goodson 
# started on 8 February 2009
# contact at jetgoodson@gmail.com
#
# general file to hold utility functions
# for character recognition, use other
# functions/macros to call
##################################

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
