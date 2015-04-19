################################################################################
#' @title createFoldsPu
#'
#' @title Data splitting functions for positive/unlabeled(PU)-data
#'
#' @description Training/test partitions \code{createDataPartition} Create training/test splits for  Create test/training partition for PU data. 
#' The positive data is split into k groups while the unlabeled data is used 
#' completely in every fold. If you want to use standard cross-validation use
#' \code{\link{createFolds}}, which served as template for this function.
#'
#' @param y a vector of outcomes for the positive and the negative class
#' @param k an integer for the number of folds (applied to the positive class) 
#' @param positive the positive class in y. if empty the label with the smaller 
#' frequency is assumed to be the positive class. 
#' @param indepUn optional, a fraction (0<indepUn<1) specifying the fraction of or a vector of indices specifying the unlabeled samples to be used for validation. 
#' @param seed an integer in order to set a seed point
#' @seealso \code{\link{createMultiFoldsPu}}, \code{\link{createFolds}}
#' @examples
#' \dontrun{
#' 
#' ## a synthetic data set
#' data(bananas)
#' 
#' ## create a pu-adapted partiton:
#' ## leave-one-out with the positive training samples
#' ## independent training/validation sets with unlabeled samples
#' ## note that the validation samples will not be included in the final model. 
#' 
#' idx <- createFoldsPu(bananas$tr$y, 20, positive=1, 
#'            indepUn=which(bananas$tr$y==0)[1:250]) 
#' fit <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], 
#'                 index=idx)
#' pred <- predict(fit, bananas$x)
#' 
#' ### compare the TPR estimated from training/test data
#' hist(fit, pred, ylim=c(0, .25))
#' hop.pos <- holdOutPredictions(fit)$pos 
#' ### compare the TPR derived from train and test data.
#' lines( quantile(hop.pos, seq(0, 1, 0.1)), 
#'        seq(0, 1, 0.1)*.25 )
#' lines( quantile(pred[bananas$y[]==1], seq(0, 1, 0.1)), 
#'        seq(0, 1, 0.1)*.25, col="red")
#' featurespace(fit)
#' ### note: final model fitted without the unlabeled validation data
#' ### specified above by indepUn=which(bananas$tr$y==0)[1:250]
#' rownames(fit$trainingData)
#' ### note: you might want to aggregate the predictions first and then
#' ### calcualte the performance metric based on aggregated hold-out predictions
#' fit.up <- update(fit, aggregatePredictions=TRUE)
#' plot(fit.up$results$puF, fit.up$results$puFAP)
#' fit.up$results[which.max(fit.up$results$puF), ]
#' fit.up$results[which.max(fit.up$results$puFAP), ]
#' featurespace(fit)
#' colnames(fit.up$results)  
#' }
#' @export
createFoldsPu <- function (y, k, positive=NULL, indepUn=NULL, seed=NULL) {
  # until now only indepUnendent unlabeled samples are supported  

  #' y <- puFactor(
  
  y.pos <- rep(1, 20)
  y.un.tr <- rep(1, 20)
  y.un.val <- rep(1, 20)
  
  
  getFoldsForP <- function (y, label, k, seed=seed) {
    if (!is.null(seed))
      set.seed(seed)
    dummy <- createFolds(y[y==label], k=k, returnTrain = TRUE)
    idx <- which(y==label)
    folds <- lapply(dummy, function(x) idx[x]  )
  }
  
  ###########################################################################
  ###########################################################################
  ### check ...
  # y
  if ( !( identical(levels(y), c("un", "pos")) & is.ordered(y) ) ) {
    if ( is.null( positive ) ) {
      positive <- .positiveLabel(y) 
      y <- puFactor(y, positive)
    }
    else {
      y <- puFactor(y, positive)
    }
  }
  
  if (!is.null(indepUn)) {
    if (length(indepUn)==1 & indepUn[1]>0 & indepUn[1]<1) {
      if (!is.null(seed))
        set.seed(seed)
      indepUn <- sample(which(y=="un"), round(sum(y=="un")*indepUn, 0))
    }
  }
  if (is.logical(indepUn))
    indepUn <- which(indepUn)
  
  idx <- 1:length(y)
  idx.un <- which(y=='un')
  if (is.null(indepUn)) {
    idx.un.tr <- idx.un
    idx.un.val <- NULL
  } else {
    idx.un.tr <-  idx.un[!(idx.un%in%indepUn)]
    idx.un.val <- idx.un[idx.un%in%indepUn]
  }
  indexP <- createFolds(y[y=='pos'], k=k, returnTrain = TRUE)
  index <- lapply(indexP, c, idx.un.tr)
  
  indexOut <- lapply(index, function(x) which(!idx%in%x) )
  
#  return(list(index=index, indexOut=indexOut))

  return(index)
}
