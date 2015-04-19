#' @title nmm_rank_models_iteratively
#' 
#' @description Iteratively ranks the models (rows) according to different criteria (columns)  with \code{\link{nmm_rank_models}}, removes the model with the lowest rank-sum. These two steps are repeated until the last model is ranked (at the first place). It is assumed that the input \code{x} has been derived from the function \code{\link{nmm_selection_criteria}}. 
#' @param x the table returned from \code{\link{nmm_selection_criteria}}
#' @export

nmm_rank_models_iteratively <- function(x) {
  nModels <- nrow(x)
  modRows <- 1:nModels
  tm <- "average"
  # cbind(tsts$olap, rank(tsts$olap, ties.method=tm))
  
  # cbind(ranks, order(ranks$sum))
  ranks <- ranksSubset <- nmm_rank_models(x)
  removed.rowInX <- c()
  
  i=1
  idx.rm <- which.max(ranksSubset$sum)
  removed.rowInX[i] <- modRows[idx.rm]
  
  for (i in 2:nrow(x)) {
    #    i <- i+1
    ranksSubset <- nmm_rank_models(x[!(modRows%in%removed.rowInX), ])  
    idx.rm <- which.max(ranksSubset$sum)
    removed.rowInX[i] <- which(!(modRows%in%removed.rowInX))[idx.rm]
  }
  
  ranks$order <- order(ranks$sum)
  ranks$order[ranks$order] <- 1:nModels
  
  ranks$orderIter <- numeric(nModels)
  ranks$orderIter[removed.rowInX] <- nModels:1
  
  return(ranks)
}