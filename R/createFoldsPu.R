################################################################################
#' @title createFoldsPu
#'
#' @description Create test/training partition for PU data. 
#' The positive data is split into k groups while the unlabeled data is used 
#' completely in every fold. If you want to use standard cross-validation use
#' \code{\link{createFolds}}, which served as template for this function.
#'
#' @param y a vector of outcomes for the positive and the negative class
#' @param k an integer for the number of folds (applied to the positive class) 
#' @param positive the positive class in y. if empty the label with the smaller 
#' frequency is assumed to be the positive class. 
#' @param index.indep optional, the elements in y which should always been used 
#' in the test group. If not given and the indices are passed to oneClass they are 
#' randomly sampled frm the argument \code{u}. 
#' @param seed an integer in order to set a seed point
#' @seealso \code{\link{createMultiFoldsPu}}, \code{\link{createFolds}}
#' @export
createFoldsPu <- function (y, k, positive=NULL, index.indep=NULL, seed=NULL) {
  # until now only index.independent unlabeled samples are supported  
  
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
  if (is.logical(index.indep))
    index.indep <- which(index.indep)
  
  idx <- 1:length(y)
  idx.un <- which(y=='un')
  if (is.null(index.indep)) {
    idx.un.tr <- idx.un
    idx.un.val <- NULL
  } else {
    idx.un.tr <-  idx.un[!(idx.un%in%index.indep)]
    idx.un.val <- idx.un[idx.un%in%index.indep]
  }
  indexP <- createFolds(y[y=='pos'], k=k, returnTrain = TRUE)
  index <- lapply(indexP, c, idx.un.tr)
  
  indexOut <- lapply(index, function(x) which(!idx%in%x) )
  
#  return(list(index=index, indexOut=indexOut))

  return(index)
}
