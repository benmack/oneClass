#' Title
#'
#' @param nP 
#' @param nU 
#' @param nN 
#' @param seed 
#'
#' @return
#' @export
#'
#' @examples
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
  idx.n <- sample(length(y==-1), nN)
  
  y <- rep(c("pos", "un", "neg"), 
           c(length(idx.p), length(idx.u), length(idx.n)))
  if (nN==0)
    y <- puFactor(y, positive = "pos")
  else
    y <- factor(y)
  x <- data.frame(bananas$x[][c(idx.p, idx.u, idx.n), ])
  return(data.frame(y=y, x))
}