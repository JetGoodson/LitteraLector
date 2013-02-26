################################
# visualyze.R
# written by Katie Malone 
# started on 24 February 2013
# contact at cmmalone.158@gmail.com
#
# a set of tools specifically designed
# for visualizing and interpreting 
# litteralector results
##################################


getPredictionTruthTable <- function( ){

    table <- read.table( "prediction_table.txt")
    print(table)
    return(table)

}


drawPerformanceMatrix<-function( table ){
    matrix <- data.matrix(table)

    ### large values on diagonal mess up color scale--remove them
    for(i in 1:nrow(matrix)) {
        matrix[i, i] = NA
    }
    
    ### min and max values of matrix, for setting color scale
    min = min(matrix, na.rm=TRUE)
    max = max(matrix, na.rm=TRUE)

    ### color scale
    ColorRamp <- rgb( seq(0, 1, length=256), ### red
                      seq(1, 0, length=256), ### green
                      seq(1, 0, length=256)) ### blue
    ColorLevels <- seq( min, max, length=length(ColorRamp) )

    main_title <- c("SVM Digit Classification")
    x_label <- colnames(matrix)
    y_label <- rownames(matrix)
    x_title <- c("actual value")
    y_title <- c("SVM prediction")
  

    ### making space for the legend/color guide
    layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))


    ### the data
    image(1:length(x_label), 1:length(y_label), t(matrix), col=ColorRamp, xlab=x_title, ylab=y_title, zlim=c(min,max), main=main_title )
    
    ### the colorscale/legend bar thing
    image(1, ColorLevels, 
        matrix(data = ColorLevels, nrow=1),
        xlab="", ylab="", xaxt="n",
        col = ColorRamp)

    layout(1)     





#    image(matrix, xlab=c("actual value"), ylab=c("SVM prediction")) 
                                ### if you call add=TRUE
                                ### without making a plot first, it creates an error 
                                ### (google for more information)

#    image(matrix, add=TRUE, xlab=c("actual value"), ylab=c("SVM prediction"))

}
