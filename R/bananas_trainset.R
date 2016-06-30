#' bananas toy data set  
#'
#' @param nP number of positive samples
#' @param nU number of unlabeled samples
#' @param nN number of negative samples
#' @param seed seed point
#'
#' @return data frame with columns y (class label), x1 & x2 (predictor variables)
#' @export
#'
#' @examples
#' \dontrun{
#' trset <- bananas_trainset(nP=50, nU=100, nN=0, seed=0)
#' plot(trset[, -1], col=(trset$y=="un")+1)
#' 
#' trset <- bananas_trainset(nP=50, nU=0, nN=100, seed=0)
#' plot(trset[, -1], col=(trset$y=="neg")+1)
#' }
bananas_trainset <- function(nP=50, nU=100, nN=0, seed=NULL) {
  
  data(bananas)
  y <- bananas$y[]
  if(!is.null(seed))
    set.seed(seed)
  idx.p <- sample(which(y==1), nP)
  if(!is.null(seed))
    set.seed(seed)
  idx.u <- sample(length(y), nU)
  if(!is.null(seed))
    set.seed(seed)
  idx.n <- sample(which(y==-1), nN)
  
  y <- rep(c("pos", "un", "neg"), 
           c(length(idx.p), length(idx.u), length(idx.n)))
  if (nN==0)
    y <- puFactor(y, positive = "pos")
  else
    y <- factor(y)
  x <- data.frame(bananas$x[][c(idx.p, idx.u, idx.n), ])
  return(data.frame(y=y, x))
}