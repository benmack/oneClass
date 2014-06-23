################################################################################
################################################################################
.clrs <- function(type) {
  clr <- switch(type, 
                'PU' = list(pos='#2166ac', un='#e0e0e0'), 
                'PN' = list(pos='#2166ac', neg='#f4a582') )
}

.ylimForHist<- function(h, positives) {
  
  med <- median(positives, na.rm=TRUE)
  yUpperLim <- max(h$density[which.min(abs(med-h$breaks)):length(h$breaks)], na.rm=TRUE)
  ylim <- c(0, yUpperLim*2.5)
  return(ylim)

}