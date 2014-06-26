#' @name resamples.train
#' @aliases resamples
#' @aliases resamples.train
#'
#'@title ... .
#'
#' @description ... .   
#' 
#' @param x an object of class \code{\link{oneClass}} (DOES IT WORK FOR TRAIN OBJECTS?).
#' @param modParam a data frame with the desired modParameters. 
#' @param modRow the index of the model, i.e. the row in the  \code{object$results} table.
#' @param modRank the model at the modelRank-th position after sorting after the performance metric. The performance metric can be specified by \code{by}. 
#' @param metric if modRank is used, which metric should be used for the ranking?
#' @param ... ...
#' @examples
#' ### to do
#' @method resamples train
#' @export
resamples.train <- function (x, modParam=NULL, modRow=NULL, modRank=NULL, metric=NULL, ...) { # , ... for e.g. modelNames
  
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
  resamps <- resamples(modelList, ...)
}