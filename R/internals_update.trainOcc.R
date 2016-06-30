### first aggregate the hold-out predictions and calculate the performance 
.calc_performanceAP <- function(mm, object, colsParam, 
                                puSummaryFunction) { 
  dt <- holdOutPredictions( object, 
                            modParam=object$results[mm,colsParam, drop=FALSE], 
                            aggregate=TRUE)
  puSummaryFunction(dataForSummaryFunction(dt), lev=c('pos', 'un'))
}