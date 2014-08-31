################################################################################
#' update.train
#'  
#' @title Update or Re-fit a Model 
#' 
#' @description Over-write the tuning parameter selection process. 
#' You can also calculate new performance metrics given the hold-out predictions
#' are stored in \code{object}.
#'
#' @param object a object of class \code{trainOcc}.
#' @param modParam a data frame with the desired model parameters, or 
#' @param modRow the index of the model, i.e. the row in the  \code{object$results} table, or
#' @param modRank the model at the modRank-th position after sorting by a performance metric (can be specified via argumetn \code{by}). 
#' @param by when the modRank is given, the models are ranked by the metric given here. 
#' if \code{NULL} the metric in \code{object$metric} is used.
#' @param newMetric a function that can be passed to the \code{summaryFunction} argument of \code{\link{trainControl}}. 
#' This updates the \code{trainOcc$results} data frame with the new metric(s) returned from the function.
#' (Only be done when hold-out predictions are stored in \code{object}.)
#' @param aggregatePredictions The efault is \code{FALSE}, which means that the performance metric is calculated for
#' each set of hold-out predictions and the metrics are then aggregated. The mean and the standard deviation is returned in the 
#' \code{object$results} table. If \code{TRUE} the hold-out predictions are first aggregated and the \code{newMetric} is calculated once. 
#' Of course, no standard deviation of the performance metric can be calculated. 
#' Metrics calculated this way get the suffix \code{AP} in the \code{object$results} table.   
#' @param newMetricsOnly logical with default set to \code{FALSE}. Set to \code{TRUE} if the metrics already contained in the results table should be removed.
#' @param allowParallel ...
#' @param ... other arguments that can be passed to update.train
#' @return an updated trainOcc object.
#' @method update trainOcc
#' @examples
#' \dontrun{
#' data(bananas)
#' 
#' tocc <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], method="ocsvm")
#' 
#' ## update to the highest ranked model according to metric puAuc
#' tocc <- update(tocc, modRank=1, by="puAuc")
#' }
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @export
update.trainOcc <- function ( object, 
                              modParam=NULL, modRow=NULL, modRank=NULL, by=NULL, 
                              newMetric=NULL, aggregatePredictions=FALSE, 
                              newMetricsOnly=FALSE, allowParallel=TRUE, ... ) {
  
### ' @param returnResamp see \code{\link{trainControl}}
### ' @param u the unlabeled samples which should be predicted with the updated model. Can be a matrix, data.frame, \code{raster}*, or \code{rasterTiled} object.
# ' @param mask a mask can be given if \code{u} is a \code{raster*} object.
  u=NULL
  mask=NULL
  returnResamp="all" 
  
  funcCallUpdate <- match.call(expand.dots = TRUE)
  
  
  if (!is.null(newMetric)) { 
    if (!object$control$savePredictions)
      stop('Performance metrics can not be updated for objects with \'savePredictions\'=FALSE. Re-run \'trainOcc\'.')
    
    ### add option aggregatePredictions=='both'
    
    
    n.models <- nrow(object$results)
    n.param <- length(object$bestTune)
    colsParam <- 1:n.param
    
    ### required later to write new train$results table
    
    oldMetrics <- object$results[,-(1:n.param)]
    colsSD <- sapply(colnames(oldMetrics), function(x) substr(x, nchar(x)-1, nchar(x)))=='SD'
    oldMetricsSD <- oldMetrics[,colsSD]
    oldMetrics <- oldMetrics[,!colsSD]
    
    if (aggregatePredictions==TRUE) {
      
      ### first aggregate the hold-out predictions and calculate the performance 
      fun.ap <- function(mm, object, colsParam) { 
        dt <- holdOutPredictions( object, 
                                  modParam=object$results[mm,colsParam, drop=FALSE], 
                                  aggregate=TRUE)
        newMetric(.dataForSummaryFunction(dt), lev=c('pos', 'un'))
      }
      #resamples[,colsMetrics] <- t(sapply(1:nrow(resamples), fun))
      
      if (any(search()%in%"package:foreach") & allowParallel) {
        metrics <- foreach(mm=1:nrow(object$results)) %dopar% fun.ap(mm=mm, object=object, colsParam=colsParam)
        ans <- names(metrics[[1]])
        metrics <- matrix(unlist(metrics), length(metrics), length(metrics[[1]]), byrow=TRUE)
        colnames(metrics) <- ans
        ### sum(metrics.check==ans); prod(dim(ans)) ### test
      } else {
        metrics <- t(sapply(1:nrow(object$results), fun.ap, object=object, colsParam=colsParam))
      }
      
      ### aggregate (mean and SD)
      newMetrics <- metrics
      colnames(newMetrics) <- paste(colnames(newMetrics), 'AP', sep="")
      
      #and combine if desired
      if (newMetricsOnly) {
        ### add a new metric
        cl <- grep(object$metric, colnames(newMetrics))
        object$results <- cbind(object$results[,(1:n.param), drop=FALSE], 
                                newMetrics)
        if (!(length(cl)==0)) {
          object$metric <- colnames(newMetrics)[cl]
          message(paste("The new metric (\"object$metric\") has been set to", 
                        colnames(newMetrics)[cl]) )
        } else {
          message("No new metric (\"object$metric\") has been set already but the old one seems to be unavailable.\nPlease set a new one.")
        }
      } else {
        object$results <- cbind(object$results[,(1:n.param), drop=FALSE], 
                                newMetrics, oldMetrics, oldMetricsSD)
      }
      
    } else { # the default, calculate 
      
      ### ### ### ### ### ### 
      ### useful stuff
      
      index.partitions <- object$control$index
      n.partitions <- length(index.partitions)
      uniquePartitions <- unique(names(object$control$index))
      
      ### ### ### ### ### ### 
      ### initialize the resampling results 
      
      ### see what has to be saved... to handle failing resamples
      
      dt <- holdOutPredictions( object, 
                                modRow=1,
                                aggregate=TRUE)
      metric.na <- newMetric(.dataForSummaryFunction(dt), lev=c('pos', 'un'))
      metric.na[] <- NA
      
      #hop <- holdOutPredictions( object, modRow=mm, partition = 1 )
      #data <- .dataForSummaryFunction(hop$pos, hop$un, resampling=hop$resampling$name)
      #dummy <- newMetric(data, lev=c('pos', 'un'))
      #n.metrics <- length(dummy)
      #names.metrics <- names(dummy)
      
      
      ### expand.grid with the unique parameter combinations and Partitions
      resamples <- expand.grid(c(lapply(.paramList(object), unique), Resample=list(uniquePartitions)))
      
      ### and add the metric 
      #dummy <- data.frame(matrix(0, nrow(resamples), length(n.metrics)))
      #colnames(dummy) <- names.metrics
      #resamples <- cbind(resamples, dummy)
      
      ### replicate the data frame n.partition times, fill in the Resamples and 
      #resamples <- matrix(rep(t(resamples), n.partitions), ncol=ncol(resamples), byrow=TRUE)
      
      #colsMetrics <- (n.param+2):(n.param+2+n.metrics-1)
      
      
      # object$trainingData[, '.outcome'][object$control$indexOut[[6]]]
    fun <- function(rr) { 
        cat(paste(rr, '.', sep=""))
        dt <- holdOutPredictions( object, 
                                  modParam=resamples[rr,colsParam, drop=FALSE],
                                  partition = resamples$Resample[rr], 
                                  aggregate=TRUE)
        if ( length(dt$pos)==0 | length(dt$neg==0) ) { ### errors like this should actually be handled in the summaryFunction!
          return(metric.na)
        } else {
          return(newMetric(.dataForSummaryFunction(dt), lev=c('pos', 'un'))) }
    }
      
      #resamples[,colsMetrics] <- t(sapply(1:nrow(resamples), fun))
      cat(paste('TODO:', nrow(resamples), '\n'))
      metrics <- t(sapply(1:nrow(resamples), fun))

    
    if (returnResamp=='all') {
      object$resample <- metrics
    }
    
      ### aggregate (mean and SD)
      newMetrics <- aggregate( as.data.frame(metrics), by=resamples[,colsParam, drop=FALSE], FUN=function(x) mean(x, na.rm=TRUE) )
      newMetricsParam <- newMetrics[,(1:n.param), drop=FALSE]
      newMetrics <- newMetrics[,-(1:n.param)]
      newMetricsSD <- aggregate( as.data.frame(metrics), by=resamples[,colsParam, drop=FALSE], FUN=function(x) sd(x, na.rm=TRUE) )[,-(1:n.param)]
      colnames(newMetricsSD) <- paste(colnames(newMetricsSD), 'SD', sep="")
      
      ### and combine
      rw.idx <- apply ( object$results[,1:n.param, drop=FALSE], 1, function(p) which(duplicated(rbind(newMetricsParam, p), fromLast=TRUE)))

      ### check
#       tst1 <- newMetricsParam[rw.idx,]
#       tst2 <- object$results[,(1:n.param)]
#       rownames(tst1) <- rownames(tst2) <- NULL
#       identical(tst1, tst2)
#       cbind(newMetricsParam[rw.idx,], object$results[,(1:n.param)])
      
      if (newMetricsOnly) {
        object$results <- cbind(object$results[,(1:n.param), drop=FALSE], 
                                newMetrics[rw.idx,], newMetricsSD[rw.idx,])
      } else {
        object$results <- cbind(object$results[,(1:n.param), drop=FALSE], 
                                newMetrics[rw.idx,], oldMetrics, newMetricsSD[rw.idx,], oldMetricsSD)
      }


    }


  if (newMetricsOnly) {
    message ( 'Old metrics of \'object$results\' have been replaced by the new metrics but nothing else has been updated so far.' )
    } else {
      message ( 'The new metrics have been added to \'object$results\' but nothing else has been updated so far.' )
    }
   
  
  }
  ### TODO: when newMetric and u are given update the final model. 
  ### else throw a message/warning that the metric has been calculated but the 
  ### final model has not been changed.
  
  if (any(!is.null(modParam), !is.null(modRow), !is.null(modRank))) {
    funcCallOc <- object$callOc
    time.train <- object$timeOc$train
    
    mp <- modelPosition(object, modParam=modParam, modRow=modRow, modRank=modRank, by=by)
    
    ### does this help to make the updating of predUn in .constructtrainOcc work?
    
    object$callUpdateOc <- NULL
    object$dotsOc <- NULL
    object$timeOc <- NULL
    object$predUn <- NULL
    class(object) <- 'train'

    nwTrn <- update(object, param = mp$param)
    nwOc <- .constructtrainOcc(nwTrn, u=u, mask=mask, time.train=time.train, 
                                 funcCall=funcCallOc, funcCallUpdate=funcCallUpdate)
    object <- nwOc
    
  }
  return(object)
} ### end trainOcc.default ################################################



