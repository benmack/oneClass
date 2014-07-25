################################################################################
#' puSummaryWithPppDepTpr
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
#' ### to do
#' }
#' @rdname puSummary
#' @export 
puSummaryWithPppDepTpr <- function(data, lev = NULL, model = NULL) { # , metrics=c("puAuc", "puF", "tpr", "ppp")
  
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  
  require(pROC, quietly = TRUE)
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
  
  #browser()
  ### ppp at different thresholds
  pred.u <- data[data[, 'obs']=='un', 'pos']
  pred.p <- data[data[, 'obs']=='pos', 'pos']
  
  ths <- quantile(pred.p, c(0, .05, .1, .2, .3, .4, .5))
  
  pppAt.th <- c()
  for ( i in 1:length(ths) )
    pppAt.th[i] <- (sum(pred.u>=ths[i])/length(data[,"pred"]))
  names(pppAt.th) <- paste("pppAtTp", (1-c(0, .05, .1, .2, .3, .4, .5))*100, sep="")
  names(ths) <- paste("thAtTp", (1-c(0, .05, .1, .2, .3, .4, .5))*100, sep="")
   
  out <- c(tpr=tpr, ppp=ppp, puAuc=puAuc, puF=puF, negD01=negD01, pppAt.th, ths) #, "SensPu", "SpecPu")
  
  return(out)
  
}