################################################################################
#' @name trainOcc
#' 
#' @title One-class classification
#'
#' @description This function performs one-class classification with one of the 
#' implemented classifiers (one-class SVM, biased SVM, maxent). The returned object is of class
#' \code{\link{trainOcc}} for which several methods are defined which hopfully support understanding the 
#' characteristics and plausibility of the model outcome and the improve critical modelling decisions. 
#'
#' @details If you pass the argument \code{trControl} (see \code{\link{trainControl}})
#' to the one-class classifier consider the following points:
#' \itemize{ 
#' \item{make sure that it makes sense. Particularly the 
#' \code{summaryFunction} should be suitable for pu-data (see 
#' \code{\link{puSummary}})}
#' \item{\code{classProbs} has to be \code{TRUE} if the continuous outputs of 
#' the one-class classifier are required to calculate the performance metric(s)
#' such that the continuous outcomes available for the calculations }
#' \item{\code{savePredictions} and \code{returnData} should also be set to 
#' \code{TRUE}, else some of the diagnostic methods will not work. }
#'}
#'
#' @param  x a data frame with the training data. The samples are in the rows and the features in the columns.  
#' @param y a vector containing the labels/ids for the positve and unlabeled samples.
#' @param u (optional) the unlabeled samples which should be predicted with the best model. Can be a \code{matrix}, \code{data.frame}, \code{raster}, or \code{tiledRaster}.
#' @param method a one-class classification method. Implemented are "ocsvm" (one-class SVM) "biasedsvm" (biased SVM), "maxent" (a link to \code{maxent} called via the package \code{dismo} \), or a costum method that can be passed to \code{\link{train}}.
#' @param metric A performance metric for PU data used for model selection. Default for "bsvm" is "puF " and for "maxent" "puAuc".
#' @param trControl see \code{\link{train}} and \code{\link{trainControl}}. If this argument is given, make sure that it makes sense. Particularly, the summaryFunction should account for positive and unlabeled data. 
#' @param index the training indices for the resampling iterations. This will be passed 
#' to the identically names argument of the \code{\link{trainControl}} function unless the argument \code{trControl} is not \code{NULL}.
#' @param positive The positive class in \code{y}.
#' @param mask if \code{u} is a raster no-data pixels can be masked (see\code{mask} in the \code{raster} package).
#' @param allowParallel enable or disable parallel processing. Even if \code{TRUE}, parallel processing is only possible if a parallel backend is loaded and available.
#' @param ... other arguments that can be passed to train. Be careful with trainControl... !
#' @return A \code{\link{trainOcc}} object with is a child of the object \code{train}.
#' @examples
#' ### to do
#' @export
trainOcc <- function ( x, y, u=NULL, method="biasedsvm", metric=NULL, 
                       trControl=NULL, index=NULL, positive=NULL, mask=NULL, 
                       allowParallel=TRUE, ...) {
  
  funcCall <- match.call(expand.dots = TRUE)
  
  ###########################################################################
  ###########################################################################
  ### check ...
  # x y
  if (nrow(x)!=length(y))
    stop('The number of rows of x has to correspond to the length of y.')
  # y
  y <- puFactor(y, positive)
  
  
  ### -----------------------------------------------------------------------
  ### select the suitable default metric for method
  if (is.null(metric) & !is.list(method))
  {
    metric <- switch(method, 
                     "biasedsvm" = "puF", 
                     "bsvm" = "puF", 
                     "ocsvm" = "puF", 
                     "maxent" = "puAuc")
  } else if (is.null(metric) & is.list(method))
  {
    metric <- 'puAuc' ### because it is threshold independent
  }
  if ( !is.null(trControl) ) {
    #  if ( !identical(dots$trControl$summaryFunction, puSummary ) )
    #    warning("You use a performance metric different from trainOcc::puSummary. Make it makes sense with PU-data.")
  } else {
    
    if (is.null(index)) {
      trControl <- trainControl(method="cv", 
                              number=10,
                              summaryFunction = puSummary, 
                              classProbs=TRUE, 
                              savePredictions = TRUE,
                              returnResamp = "all",
                              verboseIter = FALSE, 
                              allowParallel = allowParallel)
    } else {
      trControl <- trainControl(index=index, 
                                summaryFunction = puSummary, 
                                classProbs=TRUE, 
                                savePredictions = TRUE,
                                returnResamp = "all",
                                verboseIter = FALSE, 
                                allowParallel = allowParallel)
    }
  }
  
  load(system.file("models", "models.RData", package = "oneClass"))
  
  # source("inst/models/parseModels.R")
  
  ### check ...
  ###########################################################################
  ###########################################################################
  if (!is.list(method))
  {
      method <- switch(method, 
                                   "biasedsvm" = getModelInfoOneClass("biasedsvm", regex = FALSE)[[1]],
                                   "bsvm" = getModelInfoOneClass("biasedsvm", regex = FALSE)[[1]], 
                                   "ocsvm" = getModelInfoOneClass("ocsvm", regex = FALSE)[[1]],
                                   "maxent" = getModelInfoOneClass("maxent", regex = FALSE)[[1]])
  } else 
  {
    trainOccClassifier <- method
  }
  ### -----------------------------------------------------------------------
  ### run train ...
  dong <- proc.time()
  tune <- train(x, y,
                method = method, 
                metric = metric, 
                trControl = trControl, 
                ...)
  time.train <- proc.time()-dong
  
  
  ### -----------------------------------------------------------------------
  ### collect some data from the train object an include it in the one-class
  ### object
  ### later data in $train should be accessed via functions
  
  oc <- .constructtrainOcc(x=tune, u=u, mask=mask, time.train=time.train, 
                           funcCall=funcCall, ...)
  
  
  
  return(oc)
} ### end trainOcc.default ################################################