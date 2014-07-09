################################################################################
#' fPuMaxOverTh_allModels
#'
#' ... .
#'
#' ... .
#'
#' @param x ...
#' @param rm.outlier ...
#' @return ...
#' @export
fPuMaxOverTh_allModels <- function (x, rm.outlier=.2) {
  if (class(x)=='oneClass')
    x <- x$train
  
  nModels <- nrow(x$results)
  
  ### before the loop
  rslts.models <- vector('list', nModels)  
  
  ### in the model loop
  
  for (mm in 1:nModels) {
    hop <- heldOutPredictions(x, modRow=mm)
    
    obs <- factorPu(rep(c(1, 0), c(length(hop$pos), 
                                   length(hop$un))), 
                    positive=1 )
    pos <- c(hop$pos, hop$un)  
    dt <- data.frame(obs=obs, 
                     pos=pos)
    
    if (is.numeric(rm.outlier))
      if (rm.outlier>0 & rm.outlier<1) {
        dt <- dt[!(dt$obs=='pos' & dt$pos <= quantile(hop$pos, rm.outlier)), ]
      }
        
    
    rslts.models[[mm]] <- fPuMaxOverTh(data=dt, 
                                       return.max=FALSE)
  }
  return(rslts.models)
}
