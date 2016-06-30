#' @title nmm_rank_models
#' 
#' @description Ranks the models (rows) according to different criteria (columns) and calculates the rank-sum. It is assumed that the input \code{x} has been derived from the function \code{\link{nmm_selection_criteria}}. 
#' @param x  the table returned from \code{\link{nmm_selection_criteria}}
#' @export
nmm_rank_models <- function(x) {
  nModels <- nrow(x)
  tm <- "average"
  # cbind(tsts$olap, rank(tsts$olap, ties.method=tm))
  ranks <- data.frame(
    # the distributions of the positive and unlabeled data should be different
    kruskal=rank(x$p.kruskal, ties.method=tm),
    # the means of the theoretical and empirical positive distributions 
    # should be similar 
    z=rank(-x$p.z, ties.method=tm), 
    # the variances of the theoretical and empirical positive distributions 
    # should be similar 
    chi=rank(-x$p.chi, ties.method=tm), 
    # the overlap between the positive and negative components should be small  
    olap=rank(x$olap, ties.method=tm), 
    # given the above values lead to high ranks, a model with less positive 
    # predictions should be better
    pro.pos=rank(x$pro.pos, ties.method=tm)
  )
  ranks <- cbind(ranks, sum=rowSums(ranks))
  # cbind(ranks, order(ranks$sum))
  
  ranks$order <- order(ranks$sum)
  ranks$order[ranks$order] <- 1:nModels
  return(ranks)
}