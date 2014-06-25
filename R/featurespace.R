################################################################################
#' featurespace
#'
#' @description Plot the decision values of a \code{\link{oneClass}} object in its two dimensional feature space.
#'
#' @details This function is an adaptation of the \code{\link{plot.ksvm}}.
#'
#' @param x An object of class \code{\link{oneClass}}.
#' @param thresholds a scalar or numeric vector used to plot additional contour lines.   
#' @param positive class of interest
#' @param x.tr the training data
#' @param borders names list (x, y) with two vectors of length two specifiying the 
#' borders of the feature space in horizontal/x- and vertical/y- directions.
#' @param nCells number of cells in horizontal/x- and vertical/y- directions.
#' @param ... other arguments that can be passed to \code{\link{predict}}.
#' @return ...
#' @export
featurespace <- function (x, thresholds=NULL, positive=NULL, x.tr=NULL, 
                          borders=NULL, nCells=c(100, 100), main=NULL, ...) {
  if (is.null(thresholds))
    thresholds=NULL
  
  grid <- nCells

  if ((class(x)[1]=='oneClass' | class(x)[1]=='ensemble') & is.null(x.tr)) {
    sub <- as.matrix(x$trainingData[, -3])
    y <- as.matrix(x$trainingData[, 3])
  } else if (!is.null(x.tr)) {
    yCol <- colnames(x.tr)=='y'
    sub <- as.matrix(x.tr[, !yCol])
    if (any(yCol)) {
      y <- as.matrix(x.tr[, 'y'])
    } else {
      y <- rep('pos', nrow(x.tr))
    }
  }
  
  #Lbrowser()
  if (is.null(borders)) {
    yr <- seq(min(sub[, 2]), max(sub[, 2]), length = grid[2]) # plots on y-axis
    xr <- seq(min(sub[, 1]), max(sub[, 1]), length = grid[1]) # plots on x-axis
  } else {
    yr <- seq(min(borders$y), max(borders$y), length = grid[2]) # plots on y-axis
    xr <- seq(min(borders$x), max(borders$x), length = grid[1]) # plots on y-axis
  }
  sc <- 0
  
  slice = list()
  new <- expand.grid(xr, yr)
  colnames(new) <- xylb <- colnames(sub)
  
  if (class(x)[1]=='oneClass') {
    preds <- predict(x, new, type = "prob", ...)
    if (is.null(main))
      if (is.null(main))
        main <- x$modelInfo$label
  } else if (class(x)[1]=='ensemble') {
    if (is.null(main))
      main <- x$modelInfo$label
    preds <- predict(x, new, ...)$committee
    if (any(colnames(preds)=='pos'))
      preds <- data.frame(pos=preds[, 'pos', drop=FALSE])
  } else {
    preds <- predict(x, new, ...)
  }
  
  lvl <- 37
  mymax <- max(abs(preds))
  
  mylevels <- pretty(c(0, mymax), 15)
  nl <- length(mylevels) - 2
  mycols <- c(hcl(0, 100 * (nl:0/nl)^1.3, 90 - 40 * 
                    (nl:0/nl)^1.3), rev(hcl(260, 100 * (nl:0/nl)^1.3, 
                                            90 - 40 * (nl:0/nl)^1.3)))
  mylevels <- c(-rev(mylevels[-1]), mylevels)
  index <- max(which(mylevels <= min(preds))):min(which(mylevels >= 
                                                          max(preds) ) )
  if (length(index==1))
    index <- 1:length(mylevels)
  
  
  mycols <- mycols[index]
  mylevels <- mylevels[index]
  # ymat <- ymatrix(x)
  # ymean <- mean(unique(ymat))

  filled.contour(xr, yr, matrix(as.numeric(unlist(preds)), 
                                nrow = length(xr), byrow = FALSE), col = mycols, 
                 levels = mylevels, nlevels = lvl, 
                 plot.title = title(main = main, 
                                    xlab = xylb[1], ylab = xylb[2]), 
                 plot.axes = {
                   axis(1)
                   axis(2)
                   points(sub[y=='un' , 1], sub[y=='un' , 2], 
                          pch = 4,
                          cex = 0.5) 
                   points(sub[y=='pos' , 1], sub[y=='pos' , 2], 
                          pch = 16, xpd=TRUE) 
                   if (!is.null(thresholds))
                     contour(xr, yr, matrix(as.numeric(unlist(preds)), 
                                            nrow = length(xr), byrow = FALSE), 
                             levels = thresholds, lwd=2, add=TRUE)
                   
                   if (class(x)[1]=='ensemble') {
                     for (m in 1:length(x$ensembleModels)) {
                       preds <- predict(x$ensembleModels[[m]], new)
                       contour(xr, yr, matrix(as.numeric(unlist(preds)), 
                                              nrow = length(xr), byrow = FALSE), add=TRUE, 
                               lty=5, 
                               levels=x$threshold) 
                     }
                    }
                 })
}