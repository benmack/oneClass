################################################################################
#' @name aggregate.holdOutPredictions
#'
#' @title Aggregate hold-out predictions
#'
#' @description Aggregates hold-out predictions which have been 
#' extracted from a \code{\link{trainOcc}} object with the function \code{\link{holdOutPredictions}}.  
#' 
#' @param x an object of class \code{holdOutPredictions} returned from \code{\link{holdOutPredictions}}.
#' @param ... currently unused.
#' @examples
#' \dontrun{
#' toy <- threeGaussians()
#' index <- createResample(toy$tr[, 1], times=2)
#' oc <- trainOcc(x=toy$tr[, -1], y=toy$tr[, 1], 
#'                index=index, 
#'                tuneGrid=expand.grid(sigma=c(0.1,1), ### not so large grid
#'                                     cNeg=2^seq(-5, -5, 5), 
#'                                     cMultiplier=2^seq(4, 12, 4)))
#' hop <- holdOutPredictions(oc)
#' boxplot(list(pos.r1=hop$pos$Resample1, pos.r2=hop$pos$Resample2, 
#'              un.r1=hop$un$Resample1, un.r2=hop$un$Resample2))
#' hop.agg <- aggregate(hop)
#' boxplot(list(pos=hop.agg$pos, un=hop.agg$un))
#' }
#' @method aggregate holdOutPredictions
#' @export
aggregate.holdOutPredictions <- function(x, ...) {
  # if (extracted$resampling=='cv') ### not required!
  if (x$resampling$name=='repeatedcv' | x$resampling$name=='boot') {
    x$pos <- unlist(x$pos)
    x$un <- unlist(x$un)
  } else if (x$resampling$name=='cvPu') {
    x$un <- apply(matrix(unlist(x$un), nrow=length(x$un[[1]])), 1, median)
  } else if (x$resampling$name=='repeatedcvPu') {
    x$pos <- unlist(x$pos)
    x$un <- apply(matrix(unlist(x$un), nrow=length(x$un[[1]])), 1, median)
  } else if (x$resampling$name=='bootPu') {
    x$pos <- unlist(x$pos)
    x$un <- apply(matrix(unlist(x$un), nrow=length(x$un[[1]])), 1, median)
  } 
  x$aggregate=TRUE
  class(x) <- c('aggregate', 'holdOutPredictions')
  return(x)
}
