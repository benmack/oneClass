#' Test if maxent parameters are the default 
#'
#' @param nPos Number of positive samples the default depends on.
#' @param fc Feature class
#' @param beta Beta
#'
#' @return TRUE of the nPos, beta, fc are maxxent default settings
#' @export 
maxent_is_default <- function(nPos, beta, fc) {
  
  if (is.factor(nPos))
    nPos <- as.numeric(as.character(nPos))
  fcOrder <- c("L", "Q", "H", "P", "T")
  fc <- as.character(fc)
  fc <- unlist(fcSplit <- lapply(fc, function(x) 
    paste0(fcOrder[fcOrder%in%sapply(
      1:nchar(x), function(i) substr(x, i, i))],
      collapse=""))
  )
  fc_def <- maxent_default_fc(nPos)
  return(fc==fc_def & beta == 1)
}