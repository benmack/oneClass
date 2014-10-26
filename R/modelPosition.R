################################################################################
#' @name modelPosition
#'
#' @title Get parameter(s), result table row and rank of a model.  
#' 
#' @description Given the parameter(s), results table row, or rank of a particular metric of the results table,  the corresponding 
#'  two informations are extracted.  
#' 
#' @param x an object as returned from train.
#' @param modParam data frame with the parameters of the model.
#' @param modRow the row or index of the model in the 
#' model-selection table. 
#' @param modRank the rank of the model after sorting by \code{by}.
#' @param by a character or character vector specifying by which column of the \code{x$results} table to sort.
#' If \code{NULL} the performance metric is taken from the \code{train} object. 
#' @param decreasing only when \code{modRank} is used. \code{TRUE} (default) to sort in decreasing order. 
#' can be a vector if \code{by} is a vector.
#' @return a list with the parameter, row index and rank of the model.
#' @examples
#' data(bananas)
#' oc <- trainOcc(x = bananas$tr[, -1], y = bananas$tr[, 1], 
#'                tuneGrid=expand.grid(sigma=c(.1, 1), 
#'                                     cNeg=c(0.1, 1), 
#'                                     cMultiplier=c(10, 100)))
#' ## get the position (in the results table) and parameters of model which is 
#' ## on rank 3 according to the performance metric \'puAuc\' 
#' mp3 <- modelPosition(oc, modRank=3, by="puAuc")
#' @export
modelPosition <- function(x, modParam=NULL, modRow=NULL, modRank=NULL, by=NULL, decreasing=TRUE) {
    info <- list()
    
    if (nrow(x$results)==1)
      return(list(param=x$bestTune, 
                  row=1, 
                  rank=1, 
                  metric=x$metric))
    
    if ( sum(!is.null(modParam) & !is.null(modRow) & !is.null(modRow) ) > 1 )
      warning(paste('More than one model selection arguments given.\nThe first of the sequence modParam, modRow, modRow is used to select the model.'))
    
    if ( is.null(modParam) & is.null(modRow) & is.null(modRank) & is.null(by) ) {
      modParam <- x$bestTune
    } else if ( is.null(modParam) & is.null(modRow) & is.null(modRank) & !is.null(by) ) {
      modRank <- 1
    } 
    
    if ( !is.null(modParam) ) {
      info$param <- modParam
      info$row <- c()
      info$rank <- c()
      for(i in 1:nrow(modParam)) {
        info$row[i] <- .rowFromParam(x, info$param[i, , drop=FALSE])
        info$rank[i] <- .rankFromParam(x, info$param[i, , drop=FALSE], by=by, decreasing=decreasing)
      }
      info$metric <- by
      return(info)
    } else if ( !is.null(modRow) ) {
      info$param <- .paramFromRow(x, modRow)
      info$row <- modRow
      info$rank <- c()
      for (i in 1:nrow(info$param))
        info$rank[i] <- .rankFromParam(x, info$param[i, , drop=FALSE], by=by, decreasing=decreasing)
      info$metric <- by
      return(info)
    } else if ( !is.null(modRank) ) {
      info$param <- .paramFromRank(x, modRank, by=by, decreasing=decreasing)
      info$row <- .rowFromRank(x, modRank, by=by, decreasing=decreasing)
      info$rank <- modRank
      info$metric <- by
      return(info)
    }
}
