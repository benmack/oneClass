#' @name resamples
#'
#' @title Collation and Visualization of Resampling Results (Models of one Method!). 
#'
#' @description The function returns an object of class \code{\link{resamples}} as 
#' does the function \code{resamples.default} (see \code{\link[caret]{resamples}}). 
#' \code{resamples.default} can be used to collect, analyze, and visualize a resampling 
#' results of the final models from a list of \code{train} objects. 
#' Instead, \code{resamples.trainOcc} collects resampling results of different models 
#' from the same method stored in one \code{trainOcc} object. This is useful due to the fact that the 
#' metrics used for model selection are derived from positive and unlabeled data and always have to be 
#' treated with caution.
#' 
#' @param x an object of class \code{\link{trainOcc}}.
#' @param modParam a data frame with the desired modParameters. 
#' @param modRow the index of the model, i.e. the row in the  \code{object$results} table.
#' @param modRank the model at the modelRank-th position after sorting after the performance metric. The performance metric can be specified by \code{by}. 
#' @param metric if modRank is used, which metric should be used for the ranking?
#' @param ... ...
#' @examples
#' \dontrun{
#' data(bananas)
#' oc <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], 
#'                tuneGrid=expand.grid(sigma=c(0.1,1), ### not so large grid
#'                                     cNeg=2^seq(-5, 10, 3), 
#'                                     cMultiplier=2^seq(4, 15, 2)))
#' # visualize resamples of the ten models with highest (mean) puAuc values
#' resamps <- resamples(oc, modRank=1:10, metric="puAuc")
#' bwplot (resamps, metric='puF')
#' }
#' @method resamples trainOcc
#' @export
resamples.trainOcc <- function (x, modParam=NULL, modRow=NULL, modRank=NULL, metric=NULL, ...) { # , ... for e.g. modelNames
  
  if (x$control$returnResamp != "all")
    stop("Some model did not have 'returnResamp=\"all\".")
  
  if(is.null(modParam) & is.null(modRow) & is.null(modRank))
    modRank <- 1
  
  ### extract parameters of the models
  ### when modRank is given the models with the highest metric are returned.
  ### if metric is NULL it is taken from x$metric
  mp <- modelPosition(x, modParam=modParam, modRow=modRow, modRank=modRank, by=metric)
  
  ### create list of updated models, updating here simply means
  # - setting the returnResamp to 'final' and update 'resample'
  
  class(x) <- 'train'
  
  xSkeleton <- x
  xSkeleton$time <- xSkeleton$times
  xSkeleton$control$returnResamp <- "final"
  modelList <- list()
  
  # xSkeleton$resample
  
  x$resample
  xSkeleton$resample
  
  missingResampls <- logical(nrow(mp$param))
  
  for(i in 1:nrow(mp$param)) {
    
    xSkeleton$resample <- 
      eval( parse( text=paste( 'subset(x$resample, ', 
                               paste( paste(colnames(x$bestTune), 
                                            mp$param[i, ], sep="=="), 
                                      collapse=" & "), 
                               ')' ) ) )
    
    ### unknown issue: for some models no resamples are stored ...
    if (nrow(xSkeleton$resample)==0) {
      warning(paste('A for the model', paste( paste(colnames(x$bestTune), 
                                              mp$param[i, ], sep="=="), 
                                        collapse=" & "), 
              'no resampes could be found.'))
      missingResampls[i] <- TRUE
    }
    modelList[[i]] <- xSkeleton
  }
  #browser()
  
  ### unknown issue: for some models no resamples are stored ...
  if (sum(missingResampls)==nrow(mp$param))
    stop('No resamples found for any model.')
  
  xSkeleton <- modelList[[which(!missingResampls)[1]]]
  xSkeleton$resample[, 1:(ncol(xSkeleton$resample)-length(x$bestTune)-1)] <- 
    NA
  for(i in which(missingResampls)) {
    modelList[[i]] <- xSkeleton
  }
  
  xSkeleton$resample
  
  names(modelList) <- paste('rank:', mp$rank, '|row:', mp$row, sep="")
  ### 
  resamps <- caret::resamples(modelList, ...)
}