################################################################################
#' puSummary
#'
#' Calculates performance metrics based on positive and unlabeled data.
#'
#' 
#' @description This function computes PU-performance metrics useful for model selection. 
#' NOTE: The metrics are prone of uncertainty due to the nature PU-data and the user should check the plausibility of any automatic selection. 
#' 
#' @details The following metrics are calculated and stored:
#'   \itemize{
#'   \item{tpr}{ true positive rate (from P-data) }
#'   \item{ppp}{ probability of positive prediction (from U-data) }
#'   \item{puAuc}{ The area under the ROC curve (form PU-data), see Phillips et al. (2008) }
#'   \item{puF}{  \code{(tpr^2)/ppp}, see Liu et al (2003) }
#'   \item{negD01}{ \code{-sqrt( (1-tpr)^2 + ppp^2 )}, i.e. the distance between the point c(0,1) and the c(ppp, tpr)  }
#'   }
#' From the labeled positive data the true positive rate (TPR), and thus the false negative rate (FNR), can be estimated directly. 
#' Unfortunately it is not possible to estimate the true negative rate (TNR), and thus neither the false positive rate (FPR). 
#' But we can also estimate the probability of positive prediction (PPP), i.e. the fraction of unlabeled samples predicted as positives from our model. 
#' The TPR and the PPP are useful and intuitive quantities. 
#' Assume that the i) TPR can be estimated accurately and ii) we need to select one of several candidate models which all exhibit the same TPR. 
#' We can select the model with the lowest PPP because for a given TPR (and FNR), it leads to a higher TNR (and lower FPR). See \code{\link{plot_PPPvsTPR}} \cr
#' Furthermore, the \code{puAuc}
#'  
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
#' @references 
#' Phillips, Steven J. and Dud{\'i}k, Miroslav (2008): 
#' Modeling of species distributions with Maxent: new extensions and a comprehensive evaluation. 
#' Ecography, 31, 2.\cr
#' Liu, Bing and Dai, Yang and Li, Xiaoli and Lee, Wee Sun and Yu, Philip S. (2003):
#' Building text classifiers using positive and unlabeled examples.
#' In: Intl. Conf. on Data Mining, 2003.
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
  
  out <- c(tpr=tpr, ppp=ppp, puAuc=puAuc, puF=puF, negD01=negD01) #, "SensPu", "SpecPu")
  
  return(out)
  
}