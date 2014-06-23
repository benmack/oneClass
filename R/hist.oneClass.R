################################################################################
#' hist.oneClass
#'
#' ... .
#'
#' ... .
#'
#' @param x an object of class 'oneClass'.
#' @param th draw vertical lines in the histogram, indication for a threshold.
#' @param predUn a vector of unlabeled predictions (if \code{NULL} \code{x$predUn} is used, if existing).
#' @param main an overall title for the plot.
#' @param xlab a title for the x axis.
#' @param ylab a title for the y axis.
#' @param ylim the y limits of the plot.
#' @param ... other arguments that can be passed to \code{\link{plot}}. 
#' @return ... (invisible).
#' @examples
#' ### to do
#' @method hist oneClass
#' @export
hist.oneClass <- function(x, predUn=NULL, th=NULL, ylim=NULL, ...) {
  
  if (!is.null(x$holdOut$pos) & !is.null(x$holdOut$un)) {
    hop <- list(pos = x$holdOut$pos, un = x$holdOut$un)
  } else {
    hop <- holdOutPredictions(x)
  }
  
  if (!is.null(predUn)) {
    predictive.value <- predUn
  } else if (!is.null(predUn)) {
    predictive.value <- x$predUn
  } else if ( is.null(predUn) & !is.null(x$predUn) ) {
    warning('No predicted unlabeled data found. Hold-out predictions used to build the histogram.')
    predictive.value <- c(hop$pos, hop$un)
  }
  
  h <- hist(predictive.value, plot=FALSE, breaks='Scott')
  
  ###############################################################################
  ### set defaults if necessary 
  ### ylim
  if (is.null(ylim)) {
    ylim <- .ylimForHist( h, positives=unlist(hop$pos) )
    if (any(!is.finite(ylim)))
      ylim <- c(0, max( h$density ))
  }
  
  ylim[1] <- 0-diff(c(0,ylim[2]))*.15
  plot(h, freq=FALSE, ylim=ylim, ...)
  
  clrs <- .clrs('PU')
  
  bxwx <- abs(ylim[1])*.75
  boxplot(unlist(hop$pos), frame=FALSE, axes=FALSE, y=0, horizontal=TRUE, 
          at=ylim[1]*.5, add=TRUE, boxwex=bxwx, col=clrs$pos )
  
  if ( !is.null(hop$un) ) {
    boxplot(unlist(hop$un), y=0, horizontal=TRUE, axes=FALSE, 
            at=ylim[1]*.9, add=TRUE, boxwex=bxwx, col=clrs$un) # , col=cols[1]
  }
  
  if(!is.null(th)){
    for (i in 1:length(th))
      lines(rep(th[i], 2), y=c(0, ylim[2]), lwd=3)
  }
  
  
}