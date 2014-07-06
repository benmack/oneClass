################################################################################


################################################################################
.dataForSummaryFunction <- function(hop) {
  
  obs <- puFactor( rep( c(1, 0), c(length(hop$pos), length(hop$un)) ), positive=1 )
  pos <- c(hop$pos, hop$un)
  pred <- obs
  
  pred[pos>=0] <- 'pos'
  pred[pos<0] <- 'un'
  data <- data.frame(obs=obs, pred=pred, pos=pos)
  return(data)
}

################################################################################
.is.raster <- function(x)
  (class(x)=='RasterLayer' | class(x)=='RasterStack' | class(x)=='RasterBrick')

################################################################################
.rowsOfModInPredTable <- function (x, modPos=NULL, modParam=NULL, modRow=NULL, modRank=NULL, by=NULL) {
  if (is.null(modPos))
    modPos <- modelPosition(x, modParam=modParam, modRow=modRow, modRank=modRank, by=by)
  
  ### parameter (combination) for which to ge the held out predictions
  paramList <- .paramList(x)
  
  nParam <- ncol(paramList)
  ### columns of parameters in the predict table
  paramCols <- colnames(x$pred)%in%colnames(paramList)
  
  # rowsOfSelModel <- apply(x$pred[,paramCols], 1, function(x) all(x==param))
  rowsOfSelModel <- x$pred[,which(paramCols)[1]] == modPos$param[[1]]
  if (nParam>1) {
    for (i in 2:nParam) {
      rowsOfSelModel <- cbind(rowsOfSelModel, 
                              x$pred[,which(paramCols)[i]]==modPos$param[[i]])
    }
    rowsOfSelModel <- rowSums(rowsOfSelModel)==ncol(rowsOfSelModel)
  }
  return(rowsOfSelModel)
}

################################################################################
# @param x an object of class train or trainOcc
# @return a data.frame containing the model parameters/parameter combinations evaluated during the model selection  
# @export
.paramList <- function(x) {
  
  if (inherits(x, 'train')) {
    paramNames <- colnames(x$bestTune)
    paramList <- x$results[,colnames(x$results)%in%paramNames, drop=FALSE] 
  } else {
    stop("x must be an object of class 'train' or 'trainOcc'.")    
  }
  return(paramList)
}
################################################################################
.rowFromParam <- function(x, modParam) {
  paramList <- .paramList(x)
  modRow <- apply(paramList, 1, function(x) all(x==modParam) )
  if (sum(modRow)==1) {
    modRow <- which(modRow)
  } else if (sum(modRow)==0){
    warning('No exact match found between the specified parameters and the ones of the model selection table.\nUsing the closest match.')
    modRow <- which.min( dist(rbind(modParam, paramList))[1:nrow(paramList)] )
  }
  names(modRow) <- NULL
  return(modRow)
}
################################################################################
.rankFromParam <- function(x, modParam = NULL, by = NULL, decreasing=TRUE) {
  rw <- .rowFromParam(x, modParam)
  srtd <- rownames(sort(x, printTable=FALSE, by=by, decreasing=decreasing))
  rnk <- which(as.numeric(srtd)==rw)
  return(rnk)
}
################################################################################
.rankFromRow <- function(x, modRow = NULL, by = NULL, decreasing=TRUE) {
  rnk <- .rankFromParam(x, modParam=.paramFromRow(x, modRow=modRow), by=by, decreasing=decreasing)
  return(rnk)
}
################################################################################
.paramFromRow <- function(x, modRow) {
  .paramList(x)[modRow, , drop=FALSE]
}
################################################################################
.rowFromRank <- function(x, modRank = NULL, by = NULL, decreasing=TRUE) {
  rw <- as.numeric(rownames(sort(x, printTable=FALSE, by=by, decreasing=decreasing)))[modRank]
  return(rw)
}
################################################################################
.paramFromRank <- function(x, modRank, by=NULL, decreasing=TRUE) {
  rnk <- .paramFromRow(x, modRow=.rowFromRank(x, modRank=modRank, by=by, decreasing=decreasing))
  return(rnk)
}

################################################################################
.holdOutSampleTable <- function(x, modPosition=NULL) {
  
  if(is.null(modPosition))
    modPosition <- modelPosition(x)
  
  rwsOfMdInPrdTbl <- .rowsOfModInPredTable ( x, modRow=modPosition$row )
  pred <- x$pred[rwsOfMdInPrdTbl, setdiff(colnames(x$pred), c('pred', 'un'))]
  return(pred)
}

################################################################################
.positiveLabel <- function (y) {
  
  dummy <- sapply(unique(y), function(x) c(id=x, frequency=sum(y==x)))
  id <- dummy['id',which.min(dummy['frequency', ])]
  idun <- dummy['id', which.max(dummy['frequency', ])]
  
  names(id) <- NULL
  warning(paste('Positive label not given explicitly.\nThe positive class is assumed to be the one with smaller frequency.\n', id, ' (pos): ', sum(y==id), ' samples\n', id, ' (un): ', sum(y!=id), ' samples ', sep=""))
  return(id)
}

################################################################################
