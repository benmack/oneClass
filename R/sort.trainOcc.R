#' @name sort.train
#'
#'@title Sort or order the model selection table of a \code{\link{train}} or 
#' \code{\link{trainOcc}} object.
#'
#' @description The model selection table lists the parameters of the models 
#' evaluated during model selection together with the calculated performance metrics. 
#' This function is useful to find and compare the high ranked models.
#' 
#' @param x an object of class \code{\link{train}} or \code{\link{trainOcc}}.
#' @param by a character or character vector specifying by which columns to sort. 
#' If \code{NULL} the performance metric is taken from the \code{train} object.
#' @param decreasing \code{TRUE} (default) to sort in decreasing order. can be a vector if \code{by} is a vector.
#' @param printTable \code{TRUE} for printing the  table, else \code{FALSE}
#' @param rows index of the rows to be printed (ignored if \code{printTable=FALSE}). default(\code{NULL}) prints all.
#' @param cols index of the cols to be printed (ignored if \code{printTable=FALSE}). default(\code{NULL}) prints all.
#' @param digits the number of digits to print
#' @param ... not currently used.
#' @examples
#' \dontrun{
#' data(bananas)
#' ### an underfitted model 
#' oc <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], method="ocsvm")
#' sort(oc, by="tpr")
#' }
#' @method sort train
#' @export
sort.train <- function (x, decreasing=TRUE, by=NULL, printTable=TRUE, 
                        rows=NULL, cols=NULL, 
                        digits = min(3, getOption("digits") - 3), ...) {
  #select=NULL
  #if (is.null(select))
  #  select <- 1:ncol(x$d2one$results)
  
  if (is.null(by))
    by <- x$metric
  
  if (length(by)>1 & length(decreasing==1))
    decreasing <- rep(decreasing, length(by))
  
  if (is.null(rows)) {
    rows <- 1:nrow(x$results)
  } 
  if (!is.null(rows[1]) & length(rows)==1) {
    rows=1:rows
  }
    
  if (is.null(cols[1]))
    cols <- 1:ncol(x$results)
  
  # get data to sort
  orderby <- ifelse(decreasing[1], -x$results[by[1]], x$results[by[1]])[[1]]
  
  
  if (length(by)>1) {
    for (i in 2:length(by)) {
      orderby <- cbind(orderby, 
                       ifelse(decreasing[i], -x$results[by[i]], 
                              x$results[by[i]])[[1]] )
    } }
  ordered <- x$results[do.call(order, data.frame(orderby)), ] # select
  
  if (printTable)
    print(format(ordered[rows, cols], digits = digits))
  
  invisible(ordered)
}
