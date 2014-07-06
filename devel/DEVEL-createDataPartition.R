require(trainOcc)

y <- puFactor(rep(c(1, 0), c(10, 100)))

parti <- createDataPartition(y)

colnames()

lapply(parti, function(i) rbind(train=table(y[i]),
                                test=table(y[setdiff(1:length(y), i)])) )

createDataPartitionPu <- function(y, times = 1, p = 0.5, list=TRUE, ) {
  
  
  
}