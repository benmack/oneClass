### needs to be re written

# ################################################################################
# #' @title Plot PU-performance vs. evaluation accuracies to evaluate model selection.
# #'
# #' @description ...
# #'
# #' @param x  an object of class ModelSelectionEvaluation 
# #' (see \code{\link{evaluate}}).
# #' @param fromEvaluation character specifying what to plot on the y-axis. can be one of the
# #' slots of an evaluation object (see \code{\link{ModelEvaluation-class}}). 
# #' @param fromTrain character specifying what to plot on the x-axis. can be one of the
# #' \code{colnames(x$train)}. default is the metric used when calling \code{oneClass}.
# #' @param rowNumbers if \code{TRUE} the points are annotated with the row-number (in the model selection table) 
# #' of the models, 
# #' @param ... other arguments that can be passed to \code{\link{plot}}.
# #' @examples
# #' ### to do
# #' @rdname plot.ModelSelectionEvaluation
# #' @method plot ModelSelectionEvaluation
# #' @export
# plot.ModelSelectionEvaluation <- function(x, fromEvaluation="max.kappa", fromTrain=x$metric, rowNumbers=FALSE, ...) {
# # @param text.cex numeric character expansion factor for the character size of the numbers. 
# # @param text.pos position of the label.  Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified coordinates.
# 
# #   if (is.null(fromTrain))
# #     if (x$model=='biasedsvm') {
# #       fromTrain <- 'puF' } 
# #   else if (x$model=='maxent') {
# #       fromTrain <- 'puAuc'
# #     } 
#   
#   te <- lapply(x$test, evaluate, fromEvaluation)
#   # te <- evaluate(x, th=fromEvaluation)#, ...)
#   if (any(fromEvaluation%in%c('max.kappa', 'max.CCR'))) {
#     te <- sapply(te, function (x) x$thDependent$kappa)
#   }
#   
#   tr <- x$train[,colnames(x$train)==fromTrain] 
#   #   expr <- parse(text=paste("e@", fromEvaluation, sep="")) 
#   plot(tr, te, xlab=paste(fromTrain, "(train)"), 
#        ylab=paste(fromEvaluation, "(test)"), ...)
#   
#   if (rowNumbers)
#     text(tr, te, labels=1:length(tr), pos=3, cex=.8)
#   
#   invisible(te)
# }  
