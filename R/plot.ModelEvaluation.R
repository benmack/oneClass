################################################################################
#' @name plot.ModelEvaluation
#' @aliases plot
#' 
#' @title Plot PU-performance vs. evaluation accuracies to evaluate model selection.
#'
#' @description ...
#'
#' @param x  an object of class ModelSelectionEvaluation 
#' (see \code{\link{evaluate}}).
#' @param ... other arguments passed tot \code{\link{plot}}.
#' @examples
#' ### to do
#' @method plot ModelEvaluation
#' @export
plot.ModelEvaluation <- function(x, ...) {
# @param atX character specifying what to plot on the x-axis. can be one of the
# \code{colnames(x$train)}. 
# @param atY character specifying what to plot on the y-axis. can be one of the
# slots of an evaluation object (see \code{\link{ModelEvaluation-class}}). 

  
  ### write to names data frame to get the desired default x/y axis titles
  plt <- data.frame(threshold=x@t, accuracy=x@kappa)
  
  plot(plt, lwd=2, ylim=c(0,1), type='l', ...)
  points(plt, lwd=2, ...)
  lines(x@t, x@TPR, lwd=2, lty=4) # PA
  lines(x@t, x@PPP, lwd=2, lty=5) # UA
  legend('topright', legend=c('kappa', 'PA', 'UA'), 
         lty=c(1, 4, 5), 
         pch=c(1, NA, NA),
         col=c('black'), 
         #col.pch=c('black', NA, NA), 
         lwd=2)
  # lines(x@t, x@TNR, lwd=2, col=.clrs('PN')[2], type='l')
}  
