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
#' maxent_default_fc(9)
#' maxent_default_fc(10)
#' maxent_default_fc(14)
#' maxent_default_fc(15)
#' maxent_default_fc(79)
#' maxent_default_fc(80)
#' }
#' @export
maxent_default_fc <- function(nPos) {
 if (nPos<10) {
   fc="L"
 } else if (nPos>=10 & nPos<15) {  # 
   fc="LQ"
 } else if (nPos>=15 & nPos<80) {
   fc="LQH"
 } else if (nPos>=80) {
   fc="LQHPT"
 }
 return(fc)
}