################################################################################
#' @aliases hist.trainOcc
#' @aliases hist
#' 
#' @title Diagnostic distributions plot for a \code{\link{trainOcc}} object.
#'
#' @description The histogram of the predicted unlabeled data is shown together with the 
#' hold-out predictions of the positive and unlabeled traning data (boxplots). 
#'
#' @param x an object of class \code{\link{trainOcc}}.
#' @param predUn a vector of unlabeled predictions (if \code{NULL} \code{x$predUn} is used, if existing).
#' @param th draw vertical lines in the histogram, indication for a threshold.
#' @param colsAndBreaks for a color-coded histogram a list with 
#' elements \code{colors} (vector of \code{R} colors, length n) and \code{breaks} (vector of numeric values, length n+1). 
#' @param main a title for the plot. if not given the parameters of the model are added.
#' @param ylim the y limits of the plot.
#' @param ... other arguments that can be passed to \code{\link{plot}}. 
#' @return Diagnostic distributions plot.
#' @examples
#' data(bananas)
#' ### an underfitted model 
#' oc <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], 
#'                 tuneGrid=expand.grid(sigma=0.1, 
#'                                      cNeg=0.5, 
#'                                      cMultiplier=16))
#' ### predict 10% or the unlabeled data and plot 
#' # the diagnostic distributions plot
#' # and the model in the 2D feature space 
#' set.seed(123)
#' idx.pred <- sample(400*400, 16000)
#' hist(oc, predict(oc, bananas$x[][idx.pred,]), th=0)
#' featurespace(oc, th=0)
#' 
#' ### an overfitted model 
#' oc <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], 
#'                 tuneGrid=expand.grid(sigma=1, 
#'                                      cNeg=32, 
#'                                      cMultiplier=16))
#' ### predict 10% or the unlabeled data and plot 
#' # the diagnostic distributions plot
#' # and the model in the 2D feature space 
#' set.seed(123)
#' idx.pred <- sample(400*400, 16000)
#' hist(oc, predict(oc, bananas$x[][idx.pred,]), th=0)
#' featurespace(oc, th=0)
#' 
#' ### a good model 
#' oc <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], 
#'                 tuneGrid=expand.grid(sigma=1, 
#'                                      cNeg=0.0625, 
#'                                      cMultiplier=64))
#' ### predict 10% or the unlabeled data and plot 
#' # the diagnostic distributions plot
#' # and the model in the 2D feature space 
#' set.seed(123)
#' idx.pred <- sample(400*400, 16000)
#' hist(oc, predict(oc, bananas$x[][idx.pred,]), th=0)
#' featurespace(oc, th=0)
#' @method hist trainOcc
#' @examples
#' \dontrun{
#' data(bananas)
#' ### an underfitted model 
#' oc <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], 
#'                 tuneGrid=expand.grid(sigma=0.1, 
#'                                      cNeg=0.5, 
#'                                      cMultiplier=16))
#' ### predict 10% or the unlabeled data and plot 
#' # the diagnostic distributions plot
#' # and the model in the 2D feature space 
#' set.seed(123)
#' idx.pred <- sample(400*400, 16000)
#' hist(oc, predict(oc, bananas$x[][idx.pred,]), th=0)
#' featurespace(oc, th=0)
#' 
#' ### an overfitted model 
#' oc <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], 
#'                 tuneGrid=expand.grid(sigma=1, 
#'                                     cNeg=32, 
#'                                      cMultiplier=16))
#' ### predict 10% or the unlabeled data and plot 
#' # the diagnostic distributions plot
#' # and the model in the 2D feature space 
#' set.seed(123)
#' idx.pred <- sample(400*400, 16000)
#' hist(oc, predict(oc, bananas$x[][idx.pred,]), th=0)
#' featurespace(oc, th=0)
#' 
#' ### a good model 
#' oc <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], 
#'                 tuneGrid=expand.grid(sigma=1, 
#'                                      cNeg=0.0625, 
#'                                      cMultiplier=64))
#' ### predict 10% or the unlabeled data and plot 
#' # the diagnostic distributions plot
#' # and the model in the 2D feature space 
#' set.seed(123)
#' idx.pred <- sample(400*400, 16000)
#' pred <- predict(oc, bananas$x[][idx.pred,])
#' hist(oc, pred, th=0)
#' featurespace(oc, th=0)
#' 
#' ### color coded 
#' cab=list(colors=c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", 
#'                   "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC"),
#'          breaks=c(-max(pred), seq(-1.5, 1.5, .5), max(pred)) )
#' hist(oc, pred, th=0, colsUbreaks=cab, border=NA)
#' plot(predict(oc, bananas$x), col=cab$colors, breaks=cab$breaks)
#' }
#' @export
hist.trainOcc <- function(x, predUn=NULL, th=NULL, colsAndBreaks=NULL, main=NULL, ylim=NULL, ...) {
  
  if (!is.null(x$holdOut$pos) & !is.null(x$holdOut$un)) {
    hop <- list(pos = x$holdOut$pos, un = x$holdOut$un)
  } else {
    hop <- holdOutPredictions(x)
  }
  
  if (!is.null(predUn)) {
    predictive.value <- predUn
  } else if (!is.null(x$predUn)) {
    predictive.value <- x$predUn
  } else if ( is.null(predUn) & !is.null(x$predUn) ) {
    warning('No predicted unlabeled data found. Hold-out predictions used to build the histogram.')
    predictive.value <- c(hop$pos, hop$un)
  }
  
  h <- hist(predictive.value, plot=FALSE, breaks='Scott')
  h$xname <- "predictive value"
  
  ###############################################################################
  ### set defaults if necessary 
  ### ylim
  if (is.null(ylim)) {
    ylim <- .ylimForHist( h, positives=unlist(hop$pos) )
    if (any(!is.finite(ylim)))
      ylim <- c(0, max( h$density ))
  }
  
  if (is.null(main))
    main <- paste(names(x$bestTune), x$bestTune, collapse=" / ")
  
  ylim[1] <- 0-diff(c(0,ylim[2]))*.15
  
  if (!is.null(colsAndBreaks)) {
    clrs <- rep(NA, length(h$mids))
    for(i in 1:(length(colsAndBreaks$breaks)-1)) {
      idx <- h$mids>=colsAndBreaks$breaks[i] & h$mids<colsAndBreaks$breaks[i+1]
      clrs[idx] <- colsAndBreaks$colors[i]
    }
    plot(h, freq=FALSE, ylim=ylim, main=main, col=clrs, ...)
  } else {
    plot(h, freq=FALSE, ylim=ylim, main=main, ...)
  }
    
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