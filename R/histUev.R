################################################################################
#' histUev
#' @aliases histUev
#' 
#' @title Diagnostic distributions plot for a \code{\link{trainOcc}} object and threshold dependent accuracy.
#'
#' @description The histogram of the predicted unlabeled data is shown together with the 
#' hold-out predictions of the positive and unlabeled traning data (boxplots). The threshold dependent accuracy (user's/producer's accuracy and kappa) is superimposed.
#'
#' @param model an object of class \code{\link{trainOcc}}.
#' @param ev an object of class \code{\link{ModelEvaluation}}.
#' @param yLimits specify the minimum and maximum points in the density scale of to which the accuracy scale should be projected. This is useful if you add the plot to another one, e.g. a histogram plot (\code{\link{hist.trainOcc}}), whose  y-axis scale is not suitable for the accuracy plot (i.e. if it is not between 0 and 1).
#' @param ... arguments that can be passed to \code{\link{hist.trainOcc}}, e.g. \code{pred}
#' @return yLimits (invisible) and the diagnostic plot with threshold depenent accuracy.
#' @export
histUev = function(model, ev, yLimits=NULL, ...){
  yl = hist(model, ...)
  if (is.null(yLimits))
    yLimits = c(0, yl$ylim[2])
  plot(ev, add=TRUE, yLimits=yLimits)
  invisible(yLimits)
}  