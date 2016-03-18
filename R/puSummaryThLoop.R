#'@title puSummaryThLoop
#'@name puSummaryThLoop
#'@description An example for a PU summary function which derives the maximum performance metric over a set of thresholds. Here 50 thresholds between the medians of the positive and unlabeled hold out predictions. 
#' @param data a data frame or matrix with columns \code{obs}, \code{pred}, and \code{pos}.
#' The first two are the binary observed, predicted outcomes and the latter one the 
#' continous outcome for the positive class. 
#' @param lev a character vector of factors levels for the response (default is \code{NULL}).
#' @param model a character string for the model name (as taken form the method argument of train (default is \code{NULL}).
#' @param thresholds thresholds for which to calcualte the performance metrics 
#' @param maximize Charater (vector). The metrics to be maximized. 
#' Must correspond to a name of the vector returned by \code{puSummary).
#' @param returnAll return metrics for all thresholds.
#' @return A vector of performance estimates.
#'@return a numeric vector containing reasonable thresholds 
#'@examples
#' \dontrun{
#'data(bananas)
#'tuneGrid <- expand.grid(sigma=c(1:10)*rep(c(.1, 1), each=10), 
#'                        nu=c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25))
#'model <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], 
#'                  method="ocsvm", tuneGrid=tuneGrid)
#'model <- update(model, aggregatePredictions=TRUE, 
#'                metric="puFAP")
#'model <- update(model, aggregatePredictions=TRUE, 
#'                puSummaryFunction = puSummaryThLoop, 
#'                metric="thPuFAP")
#'pairs(model$results[, c("puF", "puFAP", "thPuFAP")])
#'
#'mp.puF <- modelPosition(model, modRank=1, by="puF")
#'mp.puFAP <- modelPosition(model, modRank=1, by="puFAP")
#'mp.thPuFAP <- modelPosition(model, modRank=1, by="thPuFAP")
#'
#'featurespace(update(model, modRow=mp.puF$row))
#'featurespace(update(model, modRow=mp.puFAP$row))
#'featurespace(update(model, modRow=mp.thPuFAP$row))
#'pred.puF <- predict(update(model, modRow=mp.puF$row), 
#'                    bananas$x, returnRaster=FALSE)
#'pred.puFAP <- predict(update(model, modRow=mp.puFAP$row), 
#'                      bananas$x, returnRaster=FALSE)
#'pred.thPuFAP <- predict(update(model, modRow=mp.thPuFAP$row), 
#'                        bananas$x, returnRaster=FALSE)
#'hist(update(model, modRow=mp.puF$row), pred.puF)
#'hist(update(model, modRow=mp.puFAP$row), pred.puFAP)
#'hist(update(model, modRow=mp.thPuFAP$row), pred.thPuFAP)
#'abline(v=c(0, model$results[mp.thPuFAP$row, "thAP"] ))
#'}
#'@export
puSummaryThLoop <- function(data, lev = NULL, model = NULL, 
                            thresholds = NULL, 
                            maximize=c("puF", "puF1"), 
                            returnAll=FALSE) {   
  
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  
  if (is.null(thresholds)) {
    generateThresholds <- function(data) {
      th_rng <- c(quantile(data[data[, "obs"]=="un", "pos"], .5),
                  quantile(data[data[, "obs"]=="pos", "pos"], .5)) 
      thresholds <- seq(th_rng[1], th_rng[2], length.out=50)
      return(thresholds)
    }
    thresholds <- generateThresholds(data)
  }
  
  dummy <- puSummary(data)
  rtrn <- matrix(NA, length(thresholds), length(dummy))
  colnames(rtrn) <- names(dummy)
  
  for (i in seq(along.with=thresholds) ) {
    th <- thresholds[i]
    data[, "pred"] <- ifelse(data[, "pos"]>=th, "pos", "un")
    rtrn[i, ] <- puSummary(data)
  }
  rtrn <- data.frame(th = thresholds, rtrn)
  if (!returnAll) {
    #     idx.max <- which(rtrn[, "puF"]==max(rtrn[, "puF"], na.rm=TRUE))
    #     if (length(idx.max)>1)
    #       idx.max <- idx.max[1]
    #     rtrn <- rtrn[idx.max, ]
    #     names(rtrn) <- paste("th.", names(rtrn), sep="")
    #     return( c("th" = thresholds[idx.max], rtrn) )
    idx.max <- sapply(maximize, function(met) which(rtrn[, met]==max(rtrn[, met], na.rm=T))) 
    rtrn <- lapply(1:length(maximize), function(i) {
      rtrn.row <- rtrn[idx.max[i], ]
      colnames(rtrn.row) <- paste(maximize[i], colnames(rtrn.row), sep=".")
      rtrn.row
    })
    rtrn <- do.call(cbind, rtrn)
  }
  return(rtrn)
}