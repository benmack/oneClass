### Example for modelPosition

#' data(bananas)
#' oc <- trainOcc(x = bananas$tr[, -1], y = bananas$tr[, 1], 
#'                tuneGrid=expand.grid(sigma=c(.1, 1), 
#'                                     cNeg=c(0.1, 1), 
#'                                     cMultiplier=c(10, 100)))
#' ## get the position (in the results table) and parameters of model which is 
#' ## on rank 3 according to the performance metric \'negD01\' 
#' mp3 <- modelPosition(oc, modRank=3, by="negD01")
