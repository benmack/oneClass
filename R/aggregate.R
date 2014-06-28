################################################################################
#' @name aggregate
#'
#' @title Aggregate held out predictions... .
#'
#' @description ...
#'
#' @param x a list as returned from \code{\link{holdOutPredictions}}.
#' @param ... currently unused.
#' @examples
#' ### to do
#' @export
aggregate <- function(x, ...) UseMethod("aggregate") 

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
