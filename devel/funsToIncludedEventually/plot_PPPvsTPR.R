#' plot_PPPvsTPR
#' 
#' @title Plot of the probability of positive prediction versus the true positive rate.
#' 
#' @description Assumption that the true positive rate can be estimated well from the 
#' positive data, it is reasonable to assume that for a given true positive rate a model 
#' which leads to a lower probability of positive predictions is more accurate. This is because 
#' it will lead to lower fals positive predictions.  
#' 
#' @param x a trainOcc object
#' @param cexScaling scale the size of the points
#' @param metric which metric should to be used for scaling 
#' @param cex multiply cex values  
#' @param add logical, add the plot to another plot
#' @param ... other parameters passed to plot (if \code{add} is \code{FALSE}) or points (if \code{add} is \code{TRUE})
#' @examples
#' \dontrun{
#' data(bananas)
#' ### get some models: 
#' # one-class svm
#' ocsvm <- trainOcc (x = tr[, -1], y = tr[, 1], method="ocsvm", 
#'                    tuneGrid=expand.grid(sigma=c(seq(0.01, .09, .02), seq(0.1, .9, .1)), 
#'                                         nu=seq(.05,.55,.1)) )
#' # biased svm
#' biasedsvm <- trainOcc (x = tr[, -1], y = tr[, 1], method="bsvm", 
#'                        tuneGrid=expand.grid(sigma=c(0.1, 1), 
#'                                             cNeg=2^seq(-4, 2, 2), 
#'                                             cMultiplier=2^seq(4, 8, 2) ) )
#' # compare with PPPvsTPR-plot
#' plot_PPPvsTPR(ocsvm, xlim=c(0,1), ylim=c(0,1), metric="negD01")
#' plot_PPPvsTPR(biasedsvm, xlim=c(0,1), ylim=c(0,1), metric="negD01", 
#'                 add=TRUE, col="red")
#' }
#' @export
plot_PPPvsTPR <- function(x, identifyPoints=FALSE, add=FALSE, 
                            cexScaling="none", metric=x$metric, cex=1, ...) {
  
  if (is.character(metric) & metric=="none") {
    metric <- rep(1, nrow$results)
  } else if (is.character(metric))
    metric <- x$results[[metric]]
  
  if (length(metric) != length(x$results$tpr) )
    stop("Length of metric must be equal to number of rows of x$results")
  
  
  absLogScaled <- function(x) {
    ### x: fector of metric values
    ### scale between 0 1
    abs( log( 1-approx(x=range(x), y=c(0,1), x)$y ) )
  }
  
  ### plot orig metric value vs. transformed value
  # plot(oc$results[[metric]], 
  #      absLogScaled(metricVals))
  
  cexPoints <- switch(cexScaling, 
                "linear"=metric, 
                "absLogScaled"=absLogScaled(metric) )*cex
  
  
  PPP <- x$results$ppp
  TPR <- x$results$tpr
  
  if (add) {
    points(PPP, TPR, 
         cex=cexPoints, ...)
  } else {
    plot(PPP, TPR, 
         cex=cexPoints, ...)
  }

  points(rep(0, 10), rep(1, 10), cex=seq(1,220, 20), col="grey")
  
  if (identifyPoints)
    identify(x$results$ppp, x$results$tpr)
  
  idx <- cex>0 & is.finite(cex) & !is.na(cex)
  legend("bottomright", legend=paste(range(round(metric[idx], 2))), 
         pch=1, pt.cex=range(cex[idx]))
  invisible(cex)
}