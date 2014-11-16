################################################################################
#' dataForSummaryFunction
#' 
#' @description Creates a data frame from an object of class \code{holdOutPredictions} that can be passsed to the \code{data} argument of \code{\link{puSummary}} function.
#' 
#' @param  x an object of class \code{\link{holdOutPredictions}} or \code{\link{trainOcc}}
#' @param threshold threshold used to binarize the data
#' @return a data frame that can be passed to the \code{data} argument of \code{\link{puSummary}} function.
#' @examples
#' \dontrun{
#' data(bananas)
#' model <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], method="ocsvm")
#' hop <- holdOutPredictions(model) # returns aggregated
#' d <- dataForSummaryFunction(hop)
#' hop <- holdOutPredictions(model, partition = 1)
#' d <- dataForSummaryFunction(hop)
#' }
#' @export
dataForSummaryFunction <- function(x, threshold=0, newUn=NULL) {
  
  if (class(x)=="trainOcc") {
    x <- holdOutPredictions(x, aggregate=TRUE)
  }
  
  if (!is.null(newUn)) {
    x$un <- newUn
  }
  
  obs <- puFactor( rep( c(1, 0), c(length(x$pos), length(x$un)) ), positive=1 )
  pos <- c(x$pos, x$un)
  pred <- obs
  
  pred[pos>=threshold] <- 'pos'
  pred[pos<threshold] <- 'un'
  data <- data.frame(obs=obs, pred=pred, pos=pos)
  return(data)
}