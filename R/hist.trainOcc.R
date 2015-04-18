################################################################################
#' hist.trainOcc
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
#' @param cab for a color-coded histogram a list with 
#' elements \code{colors} (vector of \code{R} colors, length n) and \code{breaks} (vector of numeric values, length n+1). 
#' @param main a title for the plot. if not given the parameters of the model are added.
#' @param ylim the y limits of the plot.
#' @param breaks see identically named argument in \code{\link{hist}}
#' @param col a colour to be used to fill the bars.
#' @param border the color of the border around the bars.
#' @param ... other arguments that can be passed to \code{\link{plot}}. 
#' @return Diagnostic distributions plot.
#' @method hist trainOcc
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
hist.trainOcc <- function(x, predUn=NULL, th=NULL, cab=NULL, main=NULL, 
                          ylim=NULL, breaks='Scott', col="grey", border=NA, 
                          xlim=NULL, ...) { # 
  if (!is.null(x$holdOut$pos) & !is.null(x$holdOut$un)) {
    hop <- list(pos = x$holdOut$pos, un = x$holdOut$un)
  } else {
    hop <- holdOutPredictions(x, aggregate=TRUE)
  }
  
  trSet_pos <- x$trainingData[x$trainingData[,ncol(x$trainingData)]=="pos", 
                              -ncol(x$trainingData)]
  pred_tr_pos <- predict(x, trSet_pos)
   
  
  if (!is.null(predUn)) {
    predictive.value <- predUn
    if (is.null(xlim))
      xlim <- range(predictive.value)
  } else if (!is.null(x$predUn)) {
    predictive.value <- x$predUn
    if (is.null(xlim))
      xlim <- range(predictive.value)
  } else if ( is.null(predUn) & is.null(x$predUn) ) {
    warning('No predicted unlabeled data found.\nUnlabeled hold-out predictions used to build the histogram.')
    predictive.value <- hop$un
    if (is.null(xlim))
      xlim <- range(c(unlist(hop$un), unlist(hop$pos)))
  }
  
  h <- hist(predictive.value, plot=FALSE, breaks=breaks, ...)
  h$xname <- "predictive value"
  
  
  ###############################################################################
  ### set defaults if necessary 
  ### ylim
  # browser()
  if (is.null(ylim)) {
    ans <- boxplot(unlist(hop$pos), plot=FALSE)$stat[1]
    maxInRelevantRange <- max(h$density[h$mids>=ans & is.finite(h$density)])
    # ylim <- .ylimForHist( h, positives=unlist(hop$pos) )
    if (maxInRelevantRange < (max(h$density[is.finite(h$density)]))) {
      ylim <- c(0, maxInRelevantRange*2) # range(h$density)
    } else {
      ylim <- c(0, maxInRelevantRange) # range(h$density)
    }
    if (any(!is.finite(ylim)))
      ylim <- c(0, max( h$density ))
  }
  
  if (is.null(main))
    main <- paste(paste(names(x$bestTune), signif(x$bestTune,3), collapse=" / "), 
                  paste("\nrow", modelPosition(x)$row, "/ #U", length(predictive.value) ) )
  
  clrs <- .clrs('PU')
  
  
  ###############################################################################
  ### ### TPR and PPP
  # browser()
  #   prb <- seq(0, 1, .01)
  #   prb.scld <- approx(c(0,1), ylim, prb)$y
  #   percentiles.pnp <- quantile(predUn, prb)
  #   percentiles.pnp.tr.un <- quantile(hop$un, prb)
  #   percentiles.tpr <- quantile(hop$pos, prb)
  
  ylim[1] <- 0-diff(c(0,ylim[2]))*.15
  if (!is.null(cab) & is.list(cab)) {
    col <- rep(NA, length(h$mids))
    for(i in 1:(length(cab$colors))) {
      if (i==1) {
        idx <- h$mids<cab$breaks[i]
      } else if (i==length(cab$colors)) {
        idx <- h$mids>=cab$breaks[i-1]
      } else {
        idx <- h$mids>=cab$breaks[i-1] & h$mids<cab$breaks[i]
      }
      col[idx] <- cab$colors[i]
    }
    
  }
  plot(h, freq=FALSE, ylim=ylim, xlim=xlim, main=main, col=col, border=border, ...)
  #   legend("topright", c("TPR", "1-PPP (train, U)", "1-PPP (all U)"), 
  #          lwd=c(2,2,2), lty=c(1,1,5), col=c("black", "black", clrs$pos))
  #   
  #   axis(4, at=approx(c(0,1), c(0, ylim[2]), c(0, .25, .5, .75, 1))$y, labels=c(0, .25, .5, .75, 1) )
  #   lines(percentiles.tpr, ylim[2]-prb.scld, lwd=2, col=clrs$pos)
  #   lines(percentiles.pnp.tr.un, ylim[2]-prb.scld, lwd=2)
  #   lines(percentiles.pnp, ylim[2]-prb.scld, lwd=2, lty=5)
  #   
  bxwx <- abs(ylim[1])*.75

  boxplot(pred_tr_pos, frame=FALSE, axes=FALSE, y=0, horizontal=TRUE, 
          at=ylim[1]*.25, add=TRUE, boxwex=bxwx, col="#a6cee3" )
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
  
  invisible(list(h=h, ylim=ylim))
  
}