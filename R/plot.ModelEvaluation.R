################################################################################
#' @name plot.ModelEvaluation
#' 
#' @title Plot accuracy statistics dependent on threshold.
#'
#' @description Kappa/Sensitivity (Producer's Accuracy) and Positive Predictive Value (Users's Accuracy) are plotted.
#'
#' @param x  an object of class \code{\link[dismo]{ModelEvaluation}}. 
#' (see \code{\link{evaluate}}).
#' @param add add the plot to the active device, e.g. a histogram plot (\code{\link{hist.trainOcc}}) (default is \code{FALSE})
#' @param yLimits specify the minimum and maximum points in the density scale of to which the accuracy scale should be projected. This is useful if you add the plot to another one, e.g. a histogram plot (\code{\link{hist.trainOcc}}), whose  y-axis scale is not suitable for the accuracy plot (i.e. if it is not between 0 and 1).
#' @param ... other arguments passed to \code{\link{plot}}.
#' @examples
#' \dontrun{
#' # get a ModelEvaluation object
#' ev <- dismo::evaluate( p=rnorm(25, mean = 1), a=rnorm(100, mean = -1) )
#' plot(ev)
#' }
#' @method plot ModelEvaluation
#' @export
plot.ModelEvaluation <- function(x, add=FALSE, yLimits=c(0, 1), ...) {
# @param atX character specifying what to plot on the x-axis. can be one of the
# \code{colnames(x$train)}. 
# @param atY character specifying what to plot on the y-axis. can be one of the
# slots of an evaluation object (see \code{\link{ModelEvaluation-class}}). 

  scaleToYLimits <- function(y) {
    approx(c(0,1), yLimits, y)$y
  }
  
  ### write to names data frame to get the desired default x/y axis titles
  plt <- data.frame(threshold=x@t, accuracy=scaleToYLimits(x@kappa))
  
  if (add) {
    lines(plt, lwd=2, ylim=c(0,1), type='l', ...)
  } else {
    plot(plt, lwd=2, ylim=c(0,1), type='l', ...)
  }
  #points(plt, lwd=2, ...)
  lines(x@t, scaleToYLimits(x@TPR), lwd=2, lty=4) # PA
  lines(x@t, scaleToYLimits(x@PPP), lwd=2, lty=5) # UA
  legend('topright', legend=c('kappa', 'PA', 'UA'), 
         lty=c(1, 4, 5), 
         pch=c(1, NA, NA),
         col=c('black'), 
         #col.pch=c('black', NA, NA), 
         lwd=2)
  # lines(x@t, x@TNR, lwd=2, col=.clrs('PN')[2], type='l')
  axis(4, at=scaleToYLimits(seq(0,1,.25)), label=seq(0,1,.25))
  # mtext("accuracy", 4)
}  
