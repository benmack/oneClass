#' @name pnpAt
#' @title pnpAt
#' @description Calculate the probability of negative prediction at the minimum/lower whisker of the positive hold-out predictions for one or more models (parameter settings) stored in the \code{\link{trainOcc}} object.
#' @param x an object of class \code{\link{trainOcc}}
#' @param modRow the row in the results table corresponding to the model for which to derive the ppp.
#' @param u a data which can be predicted with the \code{x}. If given the predictions made on \code{u} are used for the calculation instead of the held out predictions stored in \code{x}
#' @return the lower whisker threshold and the corresponding probability of positive prediction at the Lower whisker 
#' @export
pnpAt <- function (x, modRow=NULL, u=NULL) {
  
  if (is.null(modRow))
    modRow <- 1:nrow(x$results)
  
  th.min=c()
  th.lw=c()
  pnp.min <- c()
  pnp.lw <- c()
  for (i in 1:length(modRow)) {
    hop <- holdOutPredictions(x, modRow=modRow[i], aggregate=TRUE)
    if (!is.null(u)) {
      hop$un <- predict(model, u)  
    }
    bpsP <- boxplot.stats(hop$pos)$stats
    bpsU <- boxplot.stats(hop$un)$stats
    th.min[i]=min(hop$pos, na.rm=TRUE)
    th.lw[i]=bpsP[1]
    pnp.min[i] <- sum(hop$un<th.min[i])/length(hop$un)
    pnp.lw[i] <- sum(hop$un<th.lw[i])/length(hop$un)
    
    sDff <- (bpsP[1]-bpsU[5])/(bpsP[5]-bpsU[1])
  }
  
  return(cbind(thMin=th.min, pnpMin=pnp.min, thLw=th.lw, pnpLw=pnp.lw, sDff=sDff))
}
