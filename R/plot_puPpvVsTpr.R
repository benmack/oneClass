#' plot_puPpvVsTpr
#' 
#' Plot the true positive rate and the PU-positive predictive value.
#' 
#' @param x a oneClass object
#' @param cexScaling scale the size of the points, usually according to a 
#' performance metrics
#' @param metric which metric should to be used for scaling 
#' @param cex multiply cex values  
#' plot_TprVsPuPpv 
#' @export
plot_puPpvVsTpr <- function(x, cexScaling="absLogScaled", metric=x$metric, cex=1, identifyPoints=FALSE, ...) {
  
  if (is.character(metric) & metric=="d01") {
    metric <- sqrt( (1-x$results$Tpr)^2 + x$results$puPpv^2 )
  } else if (is.character(metric))
    metric <- x$results[[metric]]
  
  if (length(metric) != length(x$results$Tpr) )
    stop("Length of metric must be equal to number of rows of x$results")
  
  
  absLogScaled <- function(x) {
    ### x: fector of metric values
    ### scale between 0 1
    abs( log( 1-approx(x=range(x), y=c(0,1), x)$y ) )
  }
  
  ### plot orig metric value vs. transformed value
  # plot(oc$results[[metric]], 
  #      absLogScaled(metricVals))
  
  cexPoints <- switch(cexScaling, 
                "linear"=metric, 
                "absLogScaled"=absLogScaled(metric) )*cex
  
  plot(x$results$puPpv, x$results$Tpr, 
       cex=cexPoints, ...)
  
  if (identifyPoints)
    identify(x$results$puPpv, x$results$Tpr)
  
  idx <- cex>0 & is.finite(cex) & !is.na(cex)
  legend("bottomright", legend=paste(range(round(metric[idx], 2))), 
         pch=1, pt.cex=range(cex[idx]))
  invisible(cex)
}