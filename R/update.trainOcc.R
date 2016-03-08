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
#' @param modRank the model at the modRank-th position after sorting metric a performance metric (can be specified via argumetn \code{metric}). 
#' @param metric The metric used to select the model, e.g. in conjunction with \code{modRank} or new summary functions. 
#' if \code{NULL} the metric in \code{object$metric} is used.
#' @param puSummaryFunction a function that can be passed to the \code{summaryFunction} argument of \code{\link{trainControl}}. 
#' This updates the \code{trainOcc$results} data frame with the new metric(s) returned from the function.
#' (Only be done when hold-out predictions are stored in \code{object}.)
#' @param aggregatePredictions The default is \code{FALSE}, which means that the performance metric is calculated for
#' each set of hold-out predictions and the metrics are then aggregated. The mean and the standard deviation is returned in the 
#' \code{object$results} table. If \code{TRUE} the hold-out predictions are first aggregated and the \code{puSummaryFunction} is calculated once. 
#' Of course, no standard deviation of the performance metric can be calculated. 
#' Metrics calculated this way get the suffix \code{AP} in the \code{object$results} table.   
#' @param newMetricsOnly logical with default set to \code{FALSE}. Set to \code{TRUE} if the metrics already contained in the results table should be removed.
#' @param allowParallel if true and if a parallel backend is registered the calcualtions are run parallel
#' @param verbose A logical for printing information about the updated model.
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
#' tocc <- update(tocc, modRank=1, metric="puAuc")
#' }
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @export
update.trainOcc <- function ( object, 
                              modParam=NULL, modRow=NULL, modRank=NULL, metric=NULL, 
                              puSummaryFunction=NULL, aggregatePredictions=FALSE, 
                              newMetricsOnly=FALSE, allowParallel=TRUE, 
                              verbose=FALSE, ... ) {
  
  ### ' @param returnResamp see \code{\link{trainControl}}
  ### ' @param u the unlabeled samples which should be predicted with the updated model. Can be a matrix, data.frame, \code{raster}*, or \code{rasterTiled} object.
  # ' @param mask a mask can be given if \code{u} is a \code{raster*} object.
  u=NULL
  mask=NULL
  returnResamp="all" 
  
  funcCallUpdate <- match.call(expand.dots = TRUE)
  
  info_old <- c(modelPosition(object), metric=object$metric)
  
  if (is.null(metric)) {
    if ( (aggregatePredictions & newMetricsOnly & is.null(puSummaryFunction)) | 
           (aggregatePredictions & is.null(puSummaryFunction)) ) {
      metric <- paste(object$metric, "AP", sep="")
    } else if (!is.null(puSummaryFunction)) {
      error("Please specify the argument \'metric\' if you are using \'puSummaryFunction\'.")
    } else if (!aggregatePredictions & is.null(puSummaryFunction)) {
      metric <- object$metric
    }
  }
  
  if ( is.null(puSummaryFunction) & aggregatePredictions ) {
    puSummaryFunction <- puSummary
  }
  
  if (!is.null(puSummaryFunction)) { 
    if (!object$control$savePredictions)
      stop('Performance metrics can not be updated for objects with \'savePredictions\'=FALSE. Re-run \'trainOcc\'.')
    
    ### add option aggregatePredictions=='both'
    
    n.models <- nrow(object$results)
    n.param <- length(object$bestTune)
    colsParam <- 1:n.param
    
    ### required later to write new train$results table
    
    oldMetrics <- object$results[,-(1:n.param)]
    colsSD <- sapply(colnames(oldMetrics), function(x) 
      substr(x, nchar(x)-1, nchar(x)))=='SD'
    oldMetricsSD <- oldMetrics[,colsSD]
    oldMetrics <- oldMetrics[,!colsSD]
    
    if (aggregatePredictions) {
      
      if (.foreach.exists() & allowParallel) {
        metrics <- foreach(mm=1:nrow(object$results), 
                           .packages="oneClass") %dopar% 
          .calc_performanceAP(mm=mm, object=object, 
                              colsParam=colsParam, 
                              puSummaryFunction=puSummaryFunction)
        ans <- names(metrics[[1]])
        metrics <- matrix(unlist(metrics), 
                          length(metrics), 
                          length(metrics[[1]]), byrow=TRUE)
        colnames(metrics) <- ans
        ### sum(metrics.check==ans); prod(dim(ans)) ### test
      } else {
        metrics <- t(sapply(1:nrow(object$results), 
                            .calc_performanceAP, 
                            object=object, 
                            colsParam=colsParam, 
                            puSummaryFunction=puSummaryFunction))
      }
      
      ### aggregate (mean and SD)
      puSummaryFunction <- metrics
      colnames(puSummaryFunction) <- paste(
        colnames(puSummaryFunction), 'AP', sep="")
      
      #and combine if desired
      if (newMetricsOnly) {
        ### add a new metric
        cl <- grep(object$metric, 
                   colnames(puSummaryFunction))
        object$results <- cbind(object$results[,(1:n.param), 
                                               drop=FALSE], 
                                puSummaryFunction)
      } else {
        object$results <- cbind(object$results[,(1:n.param), drop=FALSE], 
                                puSummaryFunction, oldMetrics, oldMetricsSD)
      }
      
    } else { # the default, calculate 
      
      index.partitions <- object$control$index
      n.partitions <- length(index.partitions)
      uniquePartitions <- unique(names(object$control$index))
      
      ### ### ### ### ### ### 
      ### initialize the resampling results 
      dt <- holdOutPredictions( object, 
                                modRow=1,
                                aggregate=TRUE)
      metric.na <- puSummaryFunction(dataForSummaryFunction(dt), lev=c('pos', 'un'))
      metric.na[] <- NA
      
      ### expand.grid with the unique parameter combinations and Partitions
      resamples <- expand.grid(c(lapply(.paramList(object), 
                                        unique), 
                                 Resample=list(uniquePartitions)))
      
      fun <- function(rr) { 
        cat(paste(rr, '.', sep=""))
        dt <- holdOutPredictions( object, 
                                  modParam=resamples[rr,colsParam, drop=FALSE],
                                  partition = resamples$Resample[rr], 
                                  aggregate=TRUE)
        ### the following check should be handled in the summaryFunction!
        if ( length(dt$pos)==0 | length(dt$neg==0) ) { 
          return(metric.na)
        } else {
          return(puSummaryFunction(dataForSummaryFunction(dt), lev=c('pos', 'un'))) }
      }
      
      #resamples[,colsMetrics] <- t(sapply(1:nrow(resamples), fun))
      cat(paste('TODO:', nrow(resamples), '\n'))
      metrics <- t(sapply(1:nrow(resamples), fun))
      
      if (returnResamp=='all') {
        object$resample <- metrics
      }
      
      ### aggregate (mean and SD)
      puSummaryFunction <- aggregate( as.data.frame(metrics), 
                                      metric=resamples[,colsParam, 
                                                       drop=FALSE], 
                                      FUN=function(x) mean(x, na.rm=TRUE) )
      puSummaryFunctionParam <- puSummaryFunction[,(1:n.param), drop=FALSE]
      puSummaryFunction <- puSummaryFunction[,-(1:n.param)]
      puSummaryFunctionSD <- aggregate( as.data.frame(metrics), 
                                        metric=resamples[,colsParam, drop=FALSE], 
                                        FUN=function(x) sd(x, na.rm=TRUE) )[,-(1:n.param)]
      colnames(puSummaryFunctionSD) <- paste(colnames(puSummaryFunctionSD), 'SD', sep="")
      
      ### and combine
      rw.idx <- apply(object$results[,1:n.param, drop=FALSE], 
                      1, function(p) which(duplicated(rbind(
                        puSummaryFunctionParam, p), 
                        fromLast=TRUE)))
      
      ### check
      #       tst1 <- puSummaryFunctionParam[rw.idx,]
      #       tst2 <- object$results[,(1:n.param)]
      #       rownames(tst1) <- rownames(tst2) <- NULL
      #       identical(tst1, tst2)
      #       cbind(puSummaryFunctionParam[rw.idx,], object$results[,(1:n.param)])
      
      if (newMetricsOnly) {
        object$results <- cbind(object$results[,(1:n.param), 
                                               drop=FALSE], 
                                puSummaryFunction[rw.idx,], 
                                puSummaryFunctionSD[rw.idx,])
      } else {
        object$results <- cbind(object$results[,(1:n.param), 
                                               drop=FALSE], 
                                puSummaryFunction[rw.idx,], 
                                oldMetrics, 
                                puSummaryFunctionSD[rw.idx,], 
                                oldMetricsSD)
      }
    }
  }
  
  funcCallOc <- object$callOc
  time.train <- object$timeOc$train
  
  if (!(metric %in% colnames(object$results))) {
    mp <- modelPosition(object, modRank=1, 
                        by=info_old$metric)
    warning(paste("The specified metric is not avaialable.", 
            "The parameter selection has not been updated!"))
    metric <- info_old$metric
  } else {
    mp <- modelPosition(object, modParam=modParam, 
                        modRow=modRow, modRank=modRank, 
                        by=metric)
  }
  
  object$callUpdateOc <- NULL
  object$dotsOc <- NULL
  object$timeOc <- NULL
  object$predUn <- NULL
  object$metric <- metric
  
  class(object) <- 'train'
  
  nwTrn <- update(object, param = mp$param)
  nwOc <- .constructtrainOcc(nwTrn, u=u, mask=mask, 
                             time.train=time.train, 
                             funcCall=funcCallOc, 
                             funcCallUpdate=funcCallUpdate)
  object <- nwOc
  
  #  }
  
  if (verbose) {
    cat(paste("Old / new metric:  ", info_old$metric, " / ", 
              object$metric), "\n", 
        paste("Results of old (row: ", info_old$row, 
              ") and new (row: ", mp$row, ") models:", 
              sep=""), "\n" )
    print( signif(object$results[c(info_old$row, mp$row), ], 
                  digits = 2) )    
  }
  
  return(object)
} ### end trainOcc.default ################################################



