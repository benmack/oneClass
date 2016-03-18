################################################################################
#' @title maxent_default_fc
#'
#' @title Default maxent feature classes used given the number of positive training samples  
#'
#' @description ...
#' 
#' @param nPos number of positive training samples 
#' @examples
#' \dontrun{
#' # lower and upper bounds
#' maxent_default_fc(c(-Inf, 9, 10, 14, 15, 79, 80, Inf))
#' }
#' @export
maxent_default_fc <- function(nPos) {
 fc <- character(length(nPos))
 fc[nPos<10] <- "L"
 fc[nPos>=10 & nPos<15] <- "LQ"
 fc[nPos>=15 & nPos<80] <- "LQH"
 fc[nPos>=80] <- "LQHPT"
 return(fc)
}
