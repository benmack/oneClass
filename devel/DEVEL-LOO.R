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

### merge and calculate the hops
oc <- update(oc, newMetric=puSummary, aggregatePredictions = TRUE, newMetricsOnly=TRUE)
oc <- update(oc, modRank=1, by="puFAP")
featurespace(oc, th=0)

### check if the hold out predictions are correct
hop <- holdOutPredictions(oc)
un <- list(aggregate(hop)$un)   ### as if aggregateHop=FALSE
pos <- list(hop$pos) ### just that we can use the same loop
i=1






for (i in 1:nrow(oc$results)) {
  oc <- update(oc, modRow=i)
  pred <- predict( oc, bananas$x )
  pdf(paste("devel/diagPlotsOfAllModels_", method, "_LOO/plotOfModel-", i, ".pdf", sep=""))
  hist(oc, predUn=pred, th=0, main=paste(names(oc$bestTune), oc$bestTune, collapse=" / "))
  featurespace(oc, th=0, main=paste(names(oc$bestTune), oc$bestTune, collapse=" / "))
  plot_PPPvsTPR(oc)
  points(oc$results$ppp[i], oc$results$tpr[i], pch=8, col="red", 
         main=paste(names(oc$bestTune), oc$bestTune, collapse=" / "))
  dev.off()
}



hist(oc.up, predUn=predict(oc.up, x.te))
plot(oc.up$results$tprAP, oc.def$results$tpr); abline(0,1)
plot(oc.up$results$puFAP, oc.def$results$puF); abline(0,1)

hop.agg <- aggregate(holdOutPredictions(oc.up))



