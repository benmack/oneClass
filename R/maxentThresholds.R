################################################################################
#' @name maxentThresholds
#' 
#' @title Extract logistic output thresholds.
#'
#' @description Extract several logistic output thresholds from an object of class \code{MaxEnt} as returned from \code{\link[dismo]{maxent}} or \code{trainOcc} fitted with the method \code{maxent}.
#'
#' @param x  an object of class \code{MaxEnt} as returned from \code{\link[dismo]{maxent}} or \code{trainOcc} as returned from \code{\link{trainOcc}} fitted with the method \code{maxent}. 
#' @examples
#' \dontrun{
#' ### still missing ...
#' }
#' @export
maxentThresholds <- function(x) {
  
  if (class(x)=="trainOcc") {
    x <- x$finalModel
  }
  
  if (class(x)!="MaxEnt")
    error("x must be an object of class i) MaxEnt or ii) trainOcc fitted with the method maxent.") 
  
  dummy <- x@results
  rownames(dummy)
  dummy[grep("logistic", rownames(dummy)),]
}
