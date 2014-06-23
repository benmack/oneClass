#' @name print.ModelSelectionEvaluation
#' 
#' @title Print an object of class \code{ModelSelectionEvaluation}
#' 
#' @description A table is printed with test and PU- performance metrics for the different evaluated models.
#' Test metrics: \cr
#' auc: auc
#' mxK.K: kappa at the threshold which leads to the maximum kappa
#' mxK.OA: Overall Accuracy at the threshold which leads to the maximum kappa
#' ...
#' 
#' @param x an object of class \code{ModelSelectionEvaluation}
#' @param by by which column should the table be sorted, default: \code{max.kappa}
#' @param digits number of digits to be printed
#' @param ... options passed to the generic print method.
#' 
#' @method print ModelSelectionEvaluation
#' @export
print.ModelSelectionEvaluation <- function (x, by='mxK.K', digits=2, ...) {
  
  evAtMaxK <- lapply(x$test, evaluate, 'max.kappa')
  mxK.K <- sapply(evAtMaxK, function(x) x$thDependent$kappa)
  mxK.OA <- sapply(evAtMaxK, function(x) x$thDependent$CCR)
  mxK.PA <- sapply(evAtMaxK, function(x) x$thDependent$TPR)
  mxK.UA <- sapply(evAtMaxK, function(x) x$thDependent$PPP)
  auc <- sapply(x$test, function(x) x@auc)
  
  df <- cbind(auc=auc, mxK.K=mxK.K, mxK.OA=mxK.OA, mxK.PA=mxK.PA, mxK.UA=mxK.UA, x$train)
  
  print(round(df[order(df[, by]), ], 2), ...)
  
  invisible(round(df[order(-df[, by]), ], digits=digits))
  
}