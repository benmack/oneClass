################################################################################
#' @title createResamplePu
#'
#' @description Create test/training partition for PU data. 
#' The positive data is split into k groups while the unlabeled data is used 
#' completely in every fold. If you want to use standard cross-validation use
#' \code{\link{createFolds}}, which served as template for this function.
#'
#' @param y a vector of outcomes for the positive and the negative class
#' @param times the number of bootstrap samples to create (for the positive class)
#' @param positive the positive class in y. if empty the label with the smaller 
#' frequency is assumed to be the positive class. 
#' @param index.indep optional, the elements in y which should always been used 
#' in the test group. If not given and the indices are passed to oneClass they are 
#' randomly sampled frm the argument \code{u}. 
#' @param seed an integer in order to set a seed point
#' @seealso \code{\link{createMultiFoldsPu}}, \code{\link{createFolds}}
#' @export
createResamplePu <- function (y, times=10, positive=NULL, index.indep=NULL, seed=NULL) {
  
  y <- puFactor(y, positive)
  
  if (!is.null(seed))
    if (length(seed)!=times)
      stop('If seed is given it must be be of length \'times\'')
  
  
  idx <- 1:length(y)
  idx.un <- which(y=='un')
  if (is.null(index.indep)) {
    idx.un.tr <- idx.un
    idx.un.val <- NULL
  } else {
    idx.un.tr <-  idx.un[!(idx.un%in%index.indep)]
    idx.un.val <- idx.un[idx.un%in%index.indep]
  }
  
  rtrn <- list(index=list(), indexOut=list())
  
  idx.pos <- which(y=='pos')
  if (!is.null(seed))
    set.seed(seed)
  dummy <- createResample(y[idx.pos], times=times)
  rtrn$index <- lapply(dummy, function(x) c(idx.pos[x], idx.un.tr) )
  
  rtrn$indexOut <- lapply(dummy, function(x) c(idx.pos[!(idx.pos%in%idx.pos[x])], idx.un.val) )
  
  
  #return(rtrn)
  return(rtrn$index)
  
}
