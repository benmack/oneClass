################################################################################
#' @name evaluateAtTh
#' 
#' @title Model evaluation / accuracy assessment at one particular threshold.
#'
#' @description Calculation of accuracy for specified thresholds. This returns an 
#' object which, when printed, returns the confusion matrix and some model evaluation 
#' statistics.
#' 
#' @param x  an object of class \code{ModelEvaluation},  
#' @param th the threshold 
#' @param ... arguments passed to \code{\link[dismo]{evaluate}} from the \code{dismo} package.
#' @return an object of class ModelEvaluationAtTh which returns formated confusion matrix and accuracy statistics when printed.
#' @examples
#' # get an ModelEvaluation object
#' ev <- dismo::evaluate( p=rnorm(25, mean = 1), a=rnorm(100, mean = -1) )
#' # use evaluateAtTh in order to get an 
#' evAt0 <- evaluateAtTh(ev, th=0)
#' evAtMaxK <- evaluateAtTh(ev, th='max.kappa')
#' # print the confusion matrices and some model evaluation statistics 
#' evAt0
#' evAtMaxK
#' @export 
evaluateAtTh <- function (x, th, ...) {
  
  if (class(x)!="ModelEvaluation")
    stop("\"x\" must be of class \"ModelEvaluation\"")
  
  ################################################################################
  .indexOfThInEval <- function (x, th) {
    if ( is.numeric(th) ) {
      ### get nearest threshold
      th.idx <- which.min( abs(x@t-th) )
    } else if ( is.character(th) ) {
      dummy <- strsplit(th, split="[.]")[[1]]
      prs <- parse( text = 
                      paste("which.", dummy[1], "(x@", dummy[2], ")", sep="") )
      th.idx <- eval(prs)
    }
    return(th.idx)
  }
  
  .getEvaluation <- function (x, th.idx) {
    
    thDepSlots <- c("t", "prevalence", "ODP", "CCR", "TPR", 
                    "TNR", "FPR", "FNR", "PPP", "NPP", "MCR", "OR", "kappa")
    confmat <- matrix(x@confusion[th.idx,],2,2, byrow=TRUE)
    colnames(confmat) <- c("+ (Test)", "- (Test)")
    rownames(confmat) <- c("+ (Pred)", "- (Pred)")
    
    rtrn <- list(np=x@np, 
                 na=x@na, 
                 confusion=confmat, 
                 thIndependent=data.frame(auc=x@auc, 
                                          pauc=ifelse(length(x@pauc)==1, x@pauc, NA), 
                                          cor=x@cor, pcor=x@pcor),
                 c(auc=x@auc, pauc=x@pauc, cor=x@cor, 
                   pcor=x@pcor), 
                 thDependent=c() )
    
    dummy <- data.frame(matrix(0, 1, length(thDepSlots)))
    colnames(dummy) <- thDepSlots
    
    for(i in seq(along.with=thDepSlots)) {
      slotName <- thDepSlots[i]
      expr <- parse(text=paste("x@", slotName, "[th.idx]", sep="")) 
      dummy[, slotName] <- eval(expr)
    }
    rtrn$thDependent <- dummy
    return(rtrn)
  }
  
  if ( class(x)=="ModelEvaluation" ) {
    th.idx <- .indexOfThInEval(x, th)
    rtrn <- .getEvaluation(x, th.idx)
    rtrn$th.call <- th
    rtrn$t.idx <- th.idx
    class(rtrn) <- 'ModelEvaluationAtTh'
  } else if ( class(x)== "ModelSelectionEvaluation" ) {
    rtrn <- list()
    for (i in seq(along.with=x$test)) {
      th.idx <- .indexOfThInEval(x$test[[i]], th)
      rtrn.i <- .getEvaluation(x$test[[i]], th.idx)
      rtrn.i$th.call <- th
      rtrn.i$t.idx <- th.idx
      class(rtrn.i) <- 'ModelEvaluationAtTh'
      rtrn[[i]] <- rtrn.i
    }
    names(rtrn) <- names(x$test)
    class(rtrn) <- 'ModelSelectionEvaluationAtTh'
  } else {
    stop("x must be an object of class 'ModelEvaluation' or 'ModelSelectionEvaluation'.")
  }
  
  return(rtrn)
}
