################################################################################
#' add_puSV
#' 
#' Add the performance criteria \code{puSV} to the results table of an object of class \code{\link{trainOcc}}.
#'
#' @param  x object of class \code{\link{trainOcc}}
#' @return the same object as the input but with columns \code{x$results$nSV} and \code{x$results$puSV} added. \code{nSV} is the number of support vectors and and \code{puSV=(tpr*100)/nSV}.
#' @references
#' Muñoz-Marí, Jordi and Bovolo, Francesca and Gómez-Chova, Luis and Bruzzone, Lorenzo and Camp-Valls, Gustavo (2010): 
#' Semisupervised one-class support vector machines for classification of remote sensing data.
#' IEEE TGaRS, 48, 8.\cr
#' @export
add_puSV <- function(x) {
  nSV <- .nSv.trainOcc(x)
  x$results$nSV <- nSV
  x$results$puSV <- (x$results$tpr*100)/nSV
  return(x)
}