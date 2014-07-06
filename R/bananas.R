#' @name bananas
#' @title Two-dimensional synthetic data set of two banana shaped distributions
#' @description The data is stored as \code{raster}* objects. The two features are stored
#' in \code{bananas.x} and the class information in \code{bananas$y}. This data set 
#' allows visualization of predictive models in the two-dimensional feature 
#' space.
#' @docType data
#' @usage data(bananas)
#' @format \code{bananas$x} is a two-dimensional raster stack with the features 
#' and \code{bananas.y} a one-dimensional raster with the class labels.
#' A data frame with a training set is also available (\code{bananas$tr}). 
#' @keywords datasets
#' @examples
#' \dontrun{
#' 
#' require(raster)
#' data(bananas)
#' 
#' ### plot the features and labels (raster)
#' plot(stack(bananas$x, bananas$y))
#' 
#' ### plot the training data
#' plot( bananas$tr[, -1],
#'       pch = ifelse(bananas$tr$y==1, 16, 4 ))
#' 
#' ### the training data bananas$tr has been created as follows:
#' 
#'set.seed(123456)
#'tr.cells <- c( sample( which( values( bananas$y ) == 1 ), 20 ), 
#'               sample( ncell( bananas$y ), 500 ) )
#'bananas$tr <- c( y = rep( c( 1, 0 ), c( 20, 500 ) ), 
#'                extract( bananas$x, tr.cells ) ) 
#' }
NULL