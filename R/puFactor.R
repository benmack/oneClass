################################################################################
#' puFactor
#'
#' Convert a vector in a ordert factor with levels un<pos as required when using 
#' the one-class classiifiers from \code{oneClass}.
#'
#' @param  y a vector of labels
#' @param  positive the label of the positive class in y. if not given the id with the smallest
#' frequency is used.
#' @return an ordererd factor with the levels un<pos
#' @examples
#' puFactor(rep( c( -1, -2, 1 ), c( 5, 10, 3 ) ), positive=1)
#' @export
puFactor <- function (y, positive=NULL) { # , labels=c("un", "pos")
  
  labels=c("un", "pos")
  uy <- unique(y)
  
  if ( length(uy)==1) {
    if (is.null(positive)) {
      stop('Only one unique value in \'y\'. Argument \'positive\' required for creating an pu-factor.')
    } else {
      if (positive==uy) {
        y <- factor(dummy, levels=uy, labels='pos', ordered=TRUE)
        y <- factor(y, levels=c('un', 'pos'), labels=c('un', 'pos'), ordered=TRUE)
      } else {
        y <- factor(y, levels=uy, labels='un', ordered=TRUE)
        y <- factor(y, levels=c('un', 'pos'), labels=c('un', 'pos'), ordered=TRUE)
      }
      return(y)
    }
  } else if ( ( identical(levels(y), c("un", "pos")) & is.ordered(y) ) ) {
    return(y)  
  } else if (all(sort(unique(y))==c(0, 1))) {
    if (is.logical(y)) {
      y <- factor(y, levels=c(FALSE, TRUE), labels=labels, ordered=TRUE)
    } else {
      y <- factor(y, levels=c(0, 1), labels=labels, ordered=TRUE)
    }
    return(y)
  } else  {
    if (is.null(positive))
      positive <- .positiveLabel(y)
    
    # uy <- unique(y)
    
    if (length(uy)>2) {
      y[y!=positive] <- uy[uy!=positive][1]
      uy <- unique(y)
    }
    if (y[1]==positive)
      uy <- uy[2:1]
    y <- factor(y, levels=uy, labels=labels, ordered=TRUE)
    return(y)
  }

}