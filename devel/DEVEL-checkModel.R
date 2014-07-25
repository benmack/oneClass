rm(list=ls())
require(doParallel)
require(raster)
require(oneClass)

cl <- makeCluster(detectCores()) 
registerDoParallel(cl)

data(bananas)
tr <- bananas$tr

set.seed(123)
te.idx <- sample(ncell(bananas$x), 5000)
te.y <- bananas$y[te.idx]; table(te.y)
te.x <- bananas$x[te.idx]


set.seed(123)
index <-  createFoldsPu(tr[,1], k=20, index.indep=271:520)
method <- "biasedsvm"
oc <- trainOcc (x = tr[, -1], y = tr[, 1], index=index) #,tuneGrid=getModelInfoOneClass("biasedsvm")$biasedsvm$grid()[c(1,2,17,22,29,37,79,99,142,169),])
# id <- plot_PPPvsTPR(oc, identify=TRUE)


### run a kruskal-Wallis test on predP and predU  
checkModel <- function(x, modRow=NULL, aggregateHop=FALSE) {

  if(is.null(modRow))
    modRow=modelPosition(x)$row
  
  myKruskal <- function (pos, un) {
    tst <- kruskal.test(x=c(pos, un), rep(c(1, 0), c(length(pos), length(un)) ) )
    return(c(kruskal.stat=tst$statistic, kruskal.p=tst$p.value))
  }
  
  x <- update(x, modRow=modRow)
  hop <- holdOutPredictions(x, modRow=modRow)
  
  if (aggregateHop | !is.list(hop$pos)) {
    hop <- aggregate(hop)
    hop$pos <- list(hop$pos) ### just that we can use the same loop
    hop$un <- list(hop$un)   ### as if aggregateHop=FALSE
  } 
  
  tsts <- data.frame(matrix(NA, length(hop$pos), 2))
  colnames(tsts) <- c("kruskal.stat", "kruskal.p")
  for (i in 1:length(hop$pos)) {
    tsts[i,1:2] <- myKruskal(pos=hop$pos[[i]], un=hop$un[[i]])
  }
  return(tsts)
}
oc <- update(oc, modRow=10)
tst <- checkModel(oc, modRow=modelPosition(oc)$row); tst
tsts <- t(sapply(1:nrow(oc$results), function(m) checkModel(x=oc, modRow=m)))

m=0
tsts <-c()
#for (m in 1:nrow(oc$results))
x11()
x11()

m=m+1
oc <- update(oc, modRow=m)
pred <- predict(oc, te.x)
dev.set(4);featurespace(oc, th=0)
dev.set(5);hist(oc, pred)
oc$results[m,]
tsts[m,, drop=F]


