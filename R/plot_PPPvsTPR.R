#' plot_PPPvsTPR
#' 
#' @title Plot of the probability of positive prediction versus the true positive rate.
#' 
#' @description Assumption that the true positive rate can be estimated well from the 
#' positive data, it is reasonable to assume that for a given true positive rate a model 
#' which leads to a lower probability of positive predictions is more accurate. This is because 
#' it will lead to lower fals positive predictions.
#' 
#' @param x a \code{trainOcc} object
#' @param identifyPoints logical with default \code{FALSE}. set to \code{TRUE} and click 
#' close to the points in the plot if you want to find out the model rows  
#' @param add logical, add the plot to another plot
#' @param highlightBest logical with default set to \code{TRUE} which highlights the model ranked highest by the selection criteria, i.e. the one in \code{x$metric}
#' @param xlim  x axis limits
#' @param ylim  y axis limits
#' @param ... other parameters passed to plot (if \code{add} is \code{FALSE}) or points (if \code{add} is \code{TRUE})
#' @return a scatterplot. the point corresponding to the final model in \code{x} is highlighted. 
#' @examples
#' \dontrun{
#' data(bananas)
#' ### get some models: 
#' # one-class svm
#' ocsvm <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], method="ocsvm", 
#'                    tuneGrid=expand.grid(sigma=c(seq(0.01, .09, .02), seq(0.1, .9, .1)), 
#'                                         nu=seq(.05,.55,.1)) )
#' # biased svm
#' biasedsvm <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], method="bsvm", 
#'                        tuneGrid=expand.grid(sigma=c(0.1, 1), 
#'                                             cNeg=2^seq(-4, 2, 2), 
#'                                             cMultiplier=2^seq(4, 8, 2) ) )
#' # compare with PPPvsTPR-plot
#' plot_PPPvsTPR(ocsvm)
#' plot_PPPvsTPR(biasedsvm, add=TRUE, col="red")
#' }
#' @export
plot_PPPvsTPR <- function(x, identifyPoints=FALSE, add=FALSE, highlightBest=TRUE, 
                          xlim=c(0,1), ylim=c(0,1), ...) {
  
#   absLogScaled <- function(x) {
#     ### x: factor of metric values
#     ### scale between 0 1
#     abs( log( 1-approx(x=range(x), y=c(0,1), x)$y ) )
#   }
  
  cn <- colnames(x$results)
  
  if ( !all(any(cn=="tpr") & any(cn=="ppp")) & all(any(cn=="tprAP") & any(cn=="pppAP")) ) {
    PPP <- x$results$pppAP
    TPR <- x$results$tprAP
  } else {
    PPP <- x$results$ppp
    TPR <- x$results$tpr
  }
  
  
  if (add) {
    points(PPP, TPR, xlim=c(0,1), ylim=c(0,1), ...)
  } else {
    plot(PPP, TPR, xlim=c(0,1), ylim=c(0,1), ...)
  }

  # points(rep(0, 10), rep(1, 10), cex=seq(1,220, 20), col="grey")
  
  # highlight final model 
  if (highlightBest) {
    rw <- modelPosition(x)$row
    points(PPP[rw], TPR[rw], pch=16, col="red")
  }
  
  if (identifyPoints) {
    ip <- identify(PPP, TPR, pos=FALSE)
    mp <- modelPosition(x, modRow=ip)
    mp$orderOfIdentification <- 
    return(mp)
  }
}