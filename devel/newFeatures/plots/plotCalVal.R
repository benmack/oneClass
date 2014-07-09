################################################################################
#' @name plotCalVal
#' 
#' @title Plot the held-out data versus the training data.
#'
#' @description ...
#'
#' @param x  an object of class \code{oneClass} 
#' @param data only 'P' supported at the moment
#' @examples
#' ### to do
#' @export
plotCalVal <- function(x, data='P') {
  
  pred <- data.frame(val = x$heldOutPos[[1]], 
                     cal = predict(x$train, 
                                   x$train$trainingData[x$train$trainingData[, '.outcome']=='pos', 
                                                        colnames(x$train$trainingData)!='.outcome'], 
                                   type='prob')$pos )
  xli <- yli <- range(pred)
  
  lmod <- lm(val~cal, data=pred)
  txt <- paste('val = ', round(lmod$coefficients[2], 3), ' * cal ', ifelse(lmod$coefficients[1]<0, '-', '+'),  
               abs(round(lmod$coefficients[1], 3)), '  //  R^2 = ', round(summary(lmod)$r.squared, 2), sep="")
  
  
  plot(pred$cal, pred$val, xlab='predictions on training samples (cal)', 
       ylab='held-out predictions (val))', xlim=xli, ylim=yli)
  text(xli[1], yli[2], labels=txt, adj=4)
  abline(lmod)
  abline(0, 1)
  legend('topleft', title=txt, legend="", cex=1.7, bty='n')
  
  invisible(lmod)
  
}