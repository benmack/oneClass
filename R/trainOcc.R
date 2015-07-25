################################################################################
#' @name trainOcc
#' 
#' @title Fit a one-class classification model over Different Tuning Parameters.
#'
#' @description This function calculates resampling based performance measures over a 
#' grid of tuning parameters for one of the implemented classifiers (one-class SVM, 
#' biased SVM, maxent). 
#' 
#' @details \code{trainOcc} calls \code{\link{train}} and returns an object of class
#' \code{trainOcc} which is a child of \code{train}, i.e. methods defined in \code{caret} 
#' for \code{train} can also be used. \cr
#' Via the \code{trControl} argument you can customize the way how train acts 
#' (see \code{\link{trainControl}}) but note the following (see also the example, where the (trainOcc-) defaults of
#' \code{trControl} are given):
#' \itemize{ 
#' \item{make sure that you define a suitable \code{summaryFunction} functions which 
#' defines returns metrics for positive/unlabeled data (default: \code{\link{puSummary}}).}
#' \item{\code{classProbs} has to be set to \code{TRUE} if the continuous outputs of 
#' the one-class classifier are required to calculate all performance metric(s), i.e. the ones 
#' which rely on the continuous predictions, such as the \code{puAuc}.}
#' \item{\code{savePredictions} and \code{returnResamp} should also be set to 
#' \code{TRUE} in order to make all diagnostic methods available for later analaysis. }
#'}
#'
#' @param  x a data frame with the training data. The samples are in the rows and the features in the columns.
#' @param y a vector containing the labels encoding if a sample is positive or unlabeled.
#' @param positive The positive class in \code{y}.
#' @param method a one-class classification method. Implemented are \code{ocsvm} (one-class SVM, via \code{\link[kernlab]{ksvm}}), \code{biasedsvm} (biased SVM, via \code{\link[kernlab]{ksvm}}), \code{maxent} (via \code{\link[dismo]{maxent}}), or a costum method that can be passed to \code{\link{train}}.
#' @param metric A performance metric for positive/unlabeled data used for model selection. 
#' Default for \code{ocsvm} and  \code{biasedsvm} is \code{puF} and for \code{maxent} \code{puAuc}.
#' @param trControl see \code{\link{train}} and \code{\link{trainControl}}. 
#' If this argument is given, make sure that it makes sense (see details). 
#' @param index a list of training indices for the resampling iterations. This will be passed 
#' to the identically named argument of the \code{\link{trainControl}} function unless the argument \code{trControl} is not \code{NULL}.
#' @param summaryFunction a function to compute performance metrics across resamples. This will be passed 
#' to the identically named argument of the \code{\link{trainControl}} function unless the argument \code{trControl} is not \code{NULL}.
#' @param allowParallel enable or disable parallel processing. Even if \code{TRUE}, parallel processing is only possible if a parallel backend is loaded and available.
#' @param verboseIter Logical for printing progress, does only work if parallel processing is disabled (defaults to \code{TRUE}).
#' @param ... other arguments that can be passed to train. Be careful with trainControl... !
#' @return A \code{\link{trainOcc}} object with is a child of the object \code{train}.
#' @examples
#' \dontrun{
#' ## a synthetic data set
#' data(bananas)
#' 
#' ## this is the default setting of trControl in trainOcc
#' cntrl <- trainControl(method = "cv",
#'                       number = 10,
#'                       summaryFunction = puSummary, #!
#'                       classProbs = TRUE,           #!
#'                       savePredictions = TRUE,      #!
#'                       returnResamp = "all",        #!
#'                       allowParallel = TRUE)
#' 
#' ## but lets use repeated k-fold cross-validation
#' set.seed(123)
#' rcv.idx <- createMultiFolds(puFactor(bananas$tr[,1]), k=5, times=5)
#' cntrl <- trainControl(index = rcv.idx,
#'                       summaryFunction = puSummary, 
#'                       classProbs = TRUE, 
#'                       savePredictions = TRUE,
#'                       returnResamp = "all",
#'                       allowParallel = TRUE)
#' 
#' tocc <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], trControl=cntrl)                         
#' 
#' ## be aware that the PU-performance metrics are not always choosing the 
#' ## optimal model
#' ## you may want to investigate other performance metrics stored in the 
#' ## model selection table. 
#' tocc
#' 
#' ## neatly arranged by sorting 
#' sort(tocc, by="puF") 
#' 
#' ## particularly the true positive rate (tpr) and the probability of 
#' ## positive prediction (ppp) are informative. you want to find a model
#' ## with high tpr but low ppp.
#' plot_PPPvsTPR(tocc) 
#' 
#' ## based on this plot you may want to select candidate models for more
#' ## thoroughly evaluation: use identifyPoints=TRUE
#' \dontrun{ candiModels <- plot_PPPvsTPR(tocc, identifyPoints=TRUE) }
#' 
#' ## the former assignment returns a list like the one created here: 
#' candiModels <- modelPosition(tocc, modRow=c(80, 86, 44))
#' 
#' ## plot the resampling distributions 
#' resamps <- resamples(tocc, modRow=candiModels$row)
#' bwplot(resamps, scales="free")
#' 
#' ## also the diagnostic distributions plot can be help
#' ## therefore (a large subset of ) the unlabeled data needs to be predicted 
#' tocc.m80 <- update(tocc, modRow=candiModels$row[1]) # set the final model
#' pred.m80 <- predict(tocc, bananas$x)                # predict
#' tocc.m86 <- update(tocc, modRow=candiModels$row[2])
#' pred.m86 <- predict(tocc, bananas$x) 
#' tocc.m44 <- update(tocc, modRow=candiModels$row[3])
#' pred.m44 <- predict(tocc, bananas$x) 
#' 
#' par(mfrow=c(1,3))
#' hist(tocc.m80, pred.m80, th=0)
#' hist(tocc.m86, pred.m86, th=0)
#' hist(tocc.m44, pred.m44, th=0)
#' 
#' ## here we can also see the model in the 2D feature space. this is usually
#' ## not possible because the feature space is high diminsional.
#' par(mfrow=c(1,1))
#' featurespace(tocc.m80, th=0)
#' featurespace(tocc.m86, th=0)
#' featurespace(tocc.m44, th=0)
#' }
#' @export
trainOcc <- function ( x, y, positive=NULL, method="biasedsvm", metric=NULL, 
                       trControl=NULL, index=NULL, summaryFunction=NULL, 
                       allowParallel=TRUE, verboseIter=TRUE, ...) {
  
  funcCall <- match.call(expand.dots = TRUE)
  u=NULL
  mask=NULL
  # #' @param mask if \code{u} is a raster no-data pixels can be masked (see\code{mask} in the \code{raster} package).

  # #' @param u (optional) the unlabeled samples which should be predicted with the best model. Can be a \code{matrix}, \code{data.frame}, \code{raster}, or \code{tiledRaster}.

  
  # #' @param u (optional) the unlabeled samples which should be predicted with the best model. Can be a \code{matrix}, \code{data.frame}, \code{raster}, or \code{tiledRaster}. 
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
                                verboseIter = verboseIter, 
                                allowParallel = allowParallel)
    } else {
      trControl <- trainControl(index=index, 
                                summaryFunction = puSummary, 
                                classProbs=TRUE, 
                                savePredictions = TRUE,
                                returnResamp = "all",
                                verboseIter = verboseIter, 
                                allowParallel = allowParallel)
    }
  if (!is.null(summaryFunction)) {
    trControl$summaryFunction <- summaryFunction
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
  if (method=="maxent") {
    tune <- train(x, y, method=method, metric=metric, 
                  trControl=trControl, nPos=sum(y=="pos"), ...)
  } else {
    tune <- train(x, y, method=method, metric=metric, 
                  trControl=trControl, ...)
  }
  time.train <- proc.time()-dong
  
  
  ### -----------------------------------------------------------------------
  ### collect some data from the train object an include it in the one-class
  ### object
  ### later data in $train should be accessed via functions
  
  oc <- .constructtrainOcc(x=tune, u=u, mask=mask, time.train=time.train, 
                           funcCall=funcCall, ...)
  
  ### check if resampling has been performed with puPartition and delete 
  ### the unlabeled validation data from oc$trainingData and update the 
  ### final model
  isPuPart <- .isPuPartition(oc)
  oc$isPuPartition <- isPuPart
  if (isPuPart & oc$modelInfo$label != "one-class svm") {
    oc$trainingDataValUn <- oc$trainingData[attr(isPuPart, "indexUnVal"), ]
    oc$trainingData <- oc$trainingData[-attr(isPuPart, "indexUnVal"), ]
    oc <- update(oc, modRank=1)
  }
  return(oc)
} ### end trainOcc.default ################################################