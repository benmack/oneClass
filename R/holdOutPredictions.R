################################################################################
#' @name holdOutPredictions
#'
#' @title Extract the held out predictions of a \code{\link{trainOcc}} object. .
#'
#' @description If both modParam, and modRow are \code{NULL} the held-out predictions of the 
#' final model is returned.
#'
#' @param x a \code{\link{trainOcc}} or \code{train} object.
#' @param modParam data frame with the parameters of the model.
#' @param modRow the row or index of the model in the 
#' model-selection table. 
#' @param modRank the rank of the model after sorting by \code{by}.
#' this table is located in \code{x$train$results}/\code{x$results}).
#' @param by a character or character vector specifying by which columns to sort.
#' If \code{NULL} the performance metric is taken from the \code{train} object. 
#' @param partition The default 'all' is to return all held out samples of the particular model, but a particular partition can be specified by its name (one of the sorted unique values in \code{x$train$pred}), or an integer (giving the index in the unique values in \code{x$train$pred}).
#' @param aggregate default \code{FALSE}...
#' @return held-out predicted decision values in a list with the elemetns $pos (positive held out predictions) and $un (unlabeled held out predictions). $pos and $un can be lists again depending on the resampling methods. 
#' @examples
#' data(bananas)
#' oc <- trainOcc(x = bananas$tr[, -1], y = bananas$tr[, 1], 
#'                tuneGrid=expand.grid(sigma=c(.1, 1, 10), 
#'                                      cNeg=0.0625, 
#'                                      cMultiplier=64))
#' ### hold-out predictions of the selected model
#' hop <- holdOutPredictions(oc)
#' ### extract the hold-out samples of a model corresponding to 
#' ### a aspecific row of the results table, returned when 
#' ### printing a \'trainOcc\' object. 
#' hop <- holdOutPredictions(oc, modRow=3)
#' ### extract the hold-out predictions of a partition
#' hop <- holdOutPredictions(oc, partition=3) 
#' @export
holdOutPredictions <- function (x, modParam=NULL, modRow=NULL, modRank=NULL, by=NULL, partition='all', aggregate=FALSE) { # add index.return option! this is required to calcualte 
  ### ----------------------------------------------------------------------------
  ### main fun
  
  mp <- modelPosition(x, modParam=modParam, modRow=modRow, modRank=modRank, by=by)
  heldOutTable <- .holdOutSampleTable(x, modPosition=mp)
  resMethod <- .resamplingMethod(heldOutTable)
  
  if (partition!='all'){
    if (!is.character(partition))
      partition <- sort(unique(heldOutTable$Resample))[partition]
    
      idx.partition <- which( heldOutTable$Resample%in%partition )
      heldOutTable <- heldOutTable[idx.partition, ]
  }
  
  if ( resMethod$cv & !resMethod$pu ) {
    extracted <- .extractCv(heldOutTable) 
    resMethod$name <- 'cv'
  }
  if ( resMethod$repeatedcv & !resMethod$pu ) {
    extracted <- .extractRepeatedcv(heldOutTable) 
    resMethod$name <- 'repeatedcv'
  }
  if (  resMethod$boot & !resMethod$pu  ) {
    extracted <- .extractBoot(heldOutTable)
    resMethod$name <- 'boot'
  }
  
  if ( resMethod$cv & resMethod$pu ) {
    extracted <- .extractCvPu(heldOutTable) 
    resMethod$name <- 'cvPu'
  }
  if ( resMethod$repeatedcv & resMethod$pu ) {
    extracted <- .extractRepeatedcvPu(heldOutTable) 
    resMethod$name <- 'repeatedcvPu'
  }
  if (  resMethod$boot & resMethod$pu  ) {
    extracted <- .extractBootPu(heldOutTable)
    resMethod$name <- 'bootPu'
  }
  
  extracted$resampling <- resMethod
  extracted$partition <- partition
  extracted$aggregate <- aggregate
  
  
  class(extracted) <- 'holdOutPredictions'
  
  ##############################################################################
  ### aggregation...
  if (aggregate) {
    extracted <- aggregate(extracted)
  }
  
  return(extracted)
  
}