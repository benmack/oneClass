#' @name threeGaussians
#' 
#' @title Two-dimensional toy data consisting of four Gaussian 
#' distributions.    
#' 
#' @param n integer. the number of test samples
#' @param d vector of three integers. the distance between the three negative gaussians and the positive gaussian
#' @param n.tr vector of two integer, i.e. the number of positive and unlabeled training samples
#' @param seed a seed point controlling the randomness.
#' 
#' @return a list of the toy data set with two data frames (\code{tr} and 
#' \code{te}) both containing the columns \code{y}, \code{x1}, and \code{x2}.
#' in the training set the column \code{y} in \{0, 1\} indicates if a sample 
#' belongs to the positive class (1) or if it is unlabeled (0), and in the test 
#' set \code{te} if it belongs to the positive class (1) or the negative class 
#' (0). \code{x1} and \code{x2} are the features or predictors.
#' 
#' @examples
#' \dontrun{
#'toy <- threeGaussians(seed=9)
#'
#'### plot the test set
#'par(mfrow=c(1,2))
#'plot( toy$te[ , -1 ], pch = c( 4, 16 )[ toy$te$y + 1 ] )
#'legend('topright', legend=c('pos', 'neg'), pch=c(16, 4))
#'
#'### plot the training set
#'plot( toy$tr[ , -1 ], pch = c( 4, 16 )[ toy$tr$y + 1 ] )
#'legend('topright', legend=c('pos', 'un'), pch=c(16, 4))
#'}
#' @export
threeGaussians <- function(n=200, d=2:4, n.tr=c(20, 100), seed=NULL) {
  #cls <- c('#1b9e77', '#d95f02', '#7570b3', '#e7298a')
  if (length(n)==1)
    n <- rep(n, 4)
  y.te <- rep(1:4, n)
  
  m <- rbind(c(0, 0), 
             c(1, 0)*d[1], 
             c(0, 1)*d[2], 
             c(-1, 0)*d[3])
  if (!is.null(seed))
    set.seed(seed)
  x.te <- matrix(sapply(1:8, function(i, n, m) 
    rnorm( rep(n, 2)[i], mean=m[i]), n=n, m=m ), sum(n), 2, byrow=FALSE)
  if (!is.null(seed))
    set.seed(seed+1)
  x.u <- matrix(sapply(1:8, function(i, n, m) 
    rnorm( rep(n, 2)[i], mean=m[i]), n=round(n.tr[2]*n/sum(n),0), m=m ), sum(n.tr[2]), 2, byrow=FALSE)
  if (!is.null(seed))
    set.seed(seed+2)
  x.p <- rbind(cbind( rnorm( n.tr[1],m[1,1] ), rnorm( n.tr[1],m[1,2] ) )) 
  y.tr <- rep(c(1, 0), n.tr)
  x.tr <- rbind(x.p, x.u)
  
  te <- data.frame(y=as.numeric(y.te==1), x1=x.te[,1], x2=x.te[,2])
  tr <- data.frame(y=y.tr, x1=x.tr[,1], x2=x.tr[,2])
  
  return(list(tr=tr, te=te))
}
