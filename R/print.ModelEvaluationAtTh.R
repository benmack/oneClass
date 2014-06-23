#' @name print.ModelEvaluationAtTh
#' 
#' @title Print a \code{ModelEvaluationAtTh} object. 
#' 
#' @description Print the confusion matrix, User's and Producer's Accuracies  (UA and PA), Overall Accuracy (OA), the Area under the ROC Curve (AUC) and the Kapp(K).
#' 
#' @param x an object of class \code{ModelEvaluationAtTh}
#' @param digits number of digits to be printed
#' @param ... other arguments that be passed to \code{\link{print}}.
#' @method print ModelEvaluationAtTh
#' @export
print.ModelEvaluationAtTh <- function (x, digits=0, ...) {

confmat <- x$confusion
confmat <- rbind(confmat, SUM=colSums(confmat))
confmat <- cbind(confmat, SUM=rowSums(confmat))
confmat <- cbind(confmat, 'UA[%]'=c(diag(confmat[1:2, 1:2])/rowSums(confmat[1:2, 1:2]), NA)*100)
confmat <- rbind(confmat, 'PA[%]'=c(diag(confmat[1:2, 1:2])/colSums(confmat[1:2, 1:2]), NA, NA)*100)
confmat <- rbind(confmat, '---'=rep(NA, 4))
confmat <- rbind(confmat, 'OA[%]'=c(sum(diag(confmat[1:2, 1:2])) / sum(confmat[1:2, 1:2]), rep(NA, 3))*100 )
confmat <- rbind(confmat, 'AUC[*100]'=c(x$thIndependent$auc, rep(NA, 3))*100 )
confmat <- rbind(confmat, 'K[*100]'=c(x$thDependent$kappa, rep(NA, 3) )*100 )

confmat <- round(as.table(confmat), digits)

cat( paste( 'Positive/negative (+/-) test samples:\n', x$np, '/', x$na, '\n\n') )

cat( paste( 'Confusion Matrix at threshold', round(x$thDependent$t, 4), ': \n\n') )

print(confmat, ...)

invisible(confmat)

}