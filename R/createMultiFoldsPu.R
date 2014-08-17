################################################################################
#' @title createMultiFoldsPu
#'
#' @description Create test/training partition for PU data. 
#' The positive data is split into k groups while the unlabeled data is used 
#' completely in every fold. If you want to use standard cross-validation use
#' \code{\link{createFolds}}, which served as template for this function.
#'
#' @param y a vector of outcomes for the positive and the negative class
#' @param k an integer for the number of folds (applied to the positive class) 
#' @param times the number of repetitions to create
#' @param positive the positive class in y. if empty the label with the smaller 
#' frequency is assumed to be the positive class. 
#' @param index.indep optional, the elements in y which should always been used 
#' in the test group. If not given and the indices are passed to trainOcc they are 
#' randomly sampled frm the argument \code{u}. 
#' @param seed an integer in order to set a seed point
#' @seealso \code{\link{createMultiFoldsPu}}, \code{\link{createFolds}}
#' @export
createMultiFoldsPu <- function (y, k, times=5, positive=NULL, index.indep=NULL, seed=NULL) {
  
  
  if ( !( identical(levels(y), c("un", "pos")) & is.ordered(y) ) ) {
    if ( is.null( positive ) ) {
      positive <- .positiveLabel(y) 
      y <- puFactor(y, positive)
    }
    else {
      y <- puFactor(y, positive)
    }
  }
  
  if (!is.null(seed))
    if (length(seed)!=times)
      stop('If seed is given it must be be of length \'times\'')
  
  if (!is.null(seed)) {
    multiIndex <- lapply(1:times, function(i) createFoldsPu(y=y, k=k, positive=positive, index.indep=index.indep, seed=seed[i]))
  } else {
    multiIndex <- lapply(1:times, function(i) createFoldsPu(y=y, k=k, positive=positive, index.indep=index.indep))
  }
  
  foldnames <- paste("Fold", gsub(" ", "0", format(1:k)), sep = "")
  repnames <- paste("Rep", gsub(" ", "0", format(1:times)), sep = "")
  
  nms <- paste(foldnames, rep(repnames, each=k), sep='.')
  rtrn <- vector('list',k*times)
  names(rtrn) <- nms
  
  rtrn <- list(index=list(), indexOut=list())
  for (tt in 1:times)
    for (kk in 1:k) {
      
      rtrn$index[[paste(foldnames[kk], repnames[tt], sep='.')]] <- multiIndex[[tt]][[kk]]
    }
  
  #return(rtrn)
  return(rtrn$index)
}
