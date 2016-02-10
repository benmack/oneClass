################################################################################
#' @name maxentThresholds_shortNames
#' 
#' @title Map long to short threshold names. 
#'
#' @description ...
#'
#' @param x NULL (default) or a character vector. 
#' In the first case the short and long names are printed.
#' In the last case the (matching) long names in x are converted 
#' in short names.  
#' @examples
#' \dontrun{
#' longnames <- c("X1", 
#'                "Fixed.cumulative.value.10.logistic.threshold", 
#'                "X2")
#' maxentThresholds_shortNames(longnames)
#' }
#' @export
maxentThresholds_shortNames <- function(x=NULL) {
  name_mapping <- list(Fixed.cumulative.value.1.logistic.threshold="FCV1",
                       Fixed.cumulative.value.5.logistic.threshold="FCV5",
                       Fixed.cumulative.value.10.logistic.threshold="FCV10",
                       Minimum.training.presence.logistic.threshold="xMinTP",
                       X10.percentile.training.presence.logistic.threshold="x10TP",
                       Equal.training.sensitivity.and.specificity.logistic.threshold="ETSS",
                       Maximum.training.sensitivity.plus.specificity.logistic.threshold="MTSS",
                       Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold="BTOPATV",
                       Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold="EETOD",
                       Prevalence..average.of.logistic.output.over.background.sites.="PABS")  
  if (is.null(x)) {
    cat("Short and long maxent threshold names:\n\n")
    for (i in seq_along(name_mapping)) {
      cat(names(name_mapping)[i], "\n\t =>", name_mapping[[i]], "\n")
    }
  } else {
    if (!is.character(x))
      stop("x must be a character vetcor or NULL.")
    names(name_mapping)
    rtrn <- sapply(name_mapping[x], 
                   function(nm) ifelse(is.null(nm), NA, nm))
    names(rtrn) <- NULL
    return(rtrn)
  }
}


