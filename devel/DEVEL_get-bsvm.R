rm(list=ls())
require(doParallel)
require(raster)
require(oneClass)

cl <- makeCluster(detectCores()) 
registerDoParallel(cl)

data(bananas)
tr <- bananas$tr

set.seed(123)
index <-  createFoldsPu(tr[,1], k=20, index.indep=271:520)
method <- "biasedsvm"
oc <- trainOcc (x = tr[, -1], y = tr[, 1], index=index)

graphics.off()
ids <- plot_PPPvsTPR(oc, identify=TRUE); i=0
x11()
x11()

i=i+1
oc <- update(oc, modRow=ids$row[i])
tr.pred <- predict(oc, tr[,-1])[21:270] #[271:520]
tr.pred.indep <- predict(oc, tr[,-1])[271:520]
hop <- holdOutPredictions(oc)

dev.set(4)
par(mfrow=c(2,1))
boxplot(c(hop$un, list(tr.pred)))
boxplot(c(hop$un, list(tr.pred.indep)), main="indep")

dev.set(5)
featurespace(oc)
oc$results[ids$row[i],]
tr[c(13,14,17),]