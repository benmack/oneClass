################################################################################
#' puSummary
#'
#' ... .
#'
#' ... .
#'
#' @param data a data frame or matrix with columns \code{obs}, \code{pred}, and \code{pos}.
#' The first two are the binary observed, predicted outcomes and the latter one the 
#' continous outcome for the positive class. 
#' @param lev a character vector of factors levels for the response (default is \code{NULL}).
#' @param model a character string for the model name (as taken form the method argument of train (default is \code{NULL}).
#' @return A vector of performance estimates.
#' @seealso \code{\link{postResample}}
#' @examples  \dontrun{
#' ### create a PU-data example of random numbers
#' P <- rnorm( 30, 2 )
#' U <- c( rnorm( 80 ), rnorm( 20, 2 ) )
#' d <- data.frame( obs = puFactor( rep( c( 1, 0 ), c( 30, 100 ) ), positive = 1 ), 
#'                  pred = puFactor( c(P, U)>0, positive = TRUE ),
#'                  pos = c( P, U ) )
#' puSummary(d)
#' }
#' @rdname puSummary
#' @export 
puSummary <- function(data, lev = NULL, model = NULL) { # , metrics=c("puAuc", "puF", "tpr", "ppp")
  
  #if (nrow(data)>11)
  #  browser()
  
  #if (is.na(data[1, "pred"]))
  #  browser()
  
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  
  require(pROC)
  rocObject <- try(pROC::roc(response=data$obs, predictor=data[, 'pos'], 
                             levels=c('pos', 'un')), silent = TRUE)
  puAuc <- ifelse (class(rocObject)[1] == "try-error", 0, rocObject$auc)
  
  tpr <- sum(data[,"pred"]=="pos" & data[,"obs"]=="pos")/sum(data[,"obs"]=="pos")
  ppp <- (sum(data[,"pred"]=="pos")/length(data[,"pred"]))
  puF <- (tpr^2)/ppp
  if (is.na(puF))
    puF[1] <- 0
  
  puF <- puF[[1]]
  
  negD01 <- .negD01(tpr=tpr, ppp=ppp) #.negD01:::
  
  out <- c(puAuc=puAuc, puF=puF, negD01=negD01, tpr=tpr, ppp=ppp) #, "SensPu", "SpecPu")
  
  return(out)
  
}