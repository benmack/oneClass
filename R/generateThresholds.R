#' Generate thresholds between percentiles from unlabeled and positive data    
#'
#' @param data A matrix or data frame with column "obs" and "pos".
#' "obs": factor with "un" and "pos" levels.
#' "pos": numeric with the likelihood of the positive class. 
#' @param q_min Minimum threshold as quantile of the unlabeled samples in data.
#' @param q_max Maximum threshold as quantile of the positive samples in data.
#' @param nTh Number of thresholds to be generated between the minimum and the maximum.
#'
#' @return vector of thresholds
#' @export
generateThresholds <- function(data, q_rng=c(.5, .8), nTh=50) {
  th_rng <- c(quantile(data[data[, "obs"]=="un", "pos"], q_rng[1], na.rm=TRUE),
              quantile(data[data[, "obs"]=="pos", "pos"], q_rng[2], na.rm=TRUE)) 
  thresholds <- seq(th_rng[1], th_rng[2], length.out=nTh)
  return(thresholds)
}