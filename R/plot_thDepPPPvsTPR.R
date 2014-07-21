################################################################################
#' plot_thDepPNPvsTPR
#' 
#' @title Diagnostic distributions plot for a \code{\link{trainOcc}} object.
#'
#' @description Plots the true positive rate (TPR) and the probability of negative prediction 
#' (1-PPP, probability of positive prediction) over the complete range of possible thresholds. 
#'
#' @param x an object of class \code{\link{trainOcc}}.
#' @param predUn a vector of unlabeled predictions (if \code{NULL} \code{x$predUn} is used, if existing).
#' @param th draw vertical lines in the histogram, indication for a threshold.
#' @param cab for a color-coded histogram a list with 
#' elements \code{colors} (vector of \code{R} colors, length n) and \code{breaks} (vector of numeric values, length n+1). 
#' @param main a title for the plot. if not given the parameters of the model are added.
#' @param ylim the y limits of the plot.
#' @param ... other arguments that can be passed to \code{\link{plot}}. 
#' @return Diagnostic distributions plot.
#' @method hist trainOcc
#' @examples
#' data(bananas)
#' ### a good model 
#' oc <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], 
#'                 tuneGrid=expand.grid(sigma=1, 
#'                                      cNeg=0.0625, 
#'                                      cMultiplier=64))
#' pred <- predict(oc, bananas$x[])
#' plot_thDepNPPvsTPR(oc, pred)
#' ### or added to the histogram plot
#' h <- hist(oc, pred, th=0)
#' plot_thDepNPPvsTPR(oc, pred, add=TRUE, scaleToRange=c(0, h$ylim[2]) )
#' @export
plot_thDepNPPvsTPR <- function (x, predUn=NULL, add=FALSE, scaleToRange=NULL) {
  #browser()
  clrs <- .clrs('PU')
  hop <- holdOutPredictions(x, aggregate=TRUE)
  prb <- seq(0, 1, .01)
  percentiles <- data.frame(tr.pnp = quantile(hop$un, prb),
                            tr.tpr = quantile(hop$pos, prb) )
  if (!is.null(predUn))
    percentiles$pnp <- quantile(predUn, prb)
  if (!is.null(scaleToRange)) {
    prb.orig <- prb
    prb <- approx(c(0,1), scaleToRange, prb)$y
  } else {
    scaleToRange <- c(0,1)
  }
  
  
  
  if (add) {
    ax <- axis(4, at=approx(c(0,1), c(0, scaleToRange[2]), c(0, .25, .5, .75, 1))$y, labels=c(0, .25, .5, .75, 1) )
  } else {
    ax <- NULL
  }
  if (add) {
    lines(percentiles$tr.tpr, scaleToRange[2]-prb, lwd=2, col=clrs$pos)
  } else {
    plot(percentiles$tr.tpr, scaleToRange[2]-prb, lwd=2, col=clrs$pos, type="l")
  }
  lines(percentiles$tr.pnp, scaleToRange[2]-prb, lwd=2)
  if (!is.null(predUn))
    lines(percentiles$pnp, scaleToRange[2]-prb, lwd=2, lty=5)
  
  if (is.null(predUn)) {
    leg <- legend("topright", c("TPR", "1-PPP (train, U)"), 
                  lwd=c(2,2), lty=c(1,1), col=c("black", clrs$pos))
  } else {
    leg <- legend("topright", c("TPR", "1-PPP (train, U)", "1-PPP (all U)"), 
                  lwd=c(2,2,2), lty=c(1,1,5), col=c("black", "black", clrs$pos))
  }
  
  
  
  invisible(list(legend=leg, axis=ax))
}