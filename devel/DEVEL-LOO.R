require(doParallel)
require(raster)
require(oneClass)

cl <- makeCluster(detectCores()) 
registerDoParallel(cl)

data(bananas)
x.te <- bananas$x[sample(ncell(bananas$x), 10000)]

oc.def <- trainOcc(x = bananas$tr[, -1], y = bananas$tr[, 1])

featurespace(oc.def)

source("devel/funsToIncludedEventually/createFoldsPu.R")
index <-  createFoldsPu(bananas$tr[,1], k=20, index.indep=271:520)

oc <- trainOcc(x = bananas$tr[, -1], y = bananas$tr[, 1], 
               index=index)
featurespace(oc)

### hold-out predictions of the selected model
hop <- holdOutPredictions(oc)

### merge and calculate the hops
oc.up <- update(oc, newMetric=puSummary, aggregatePredictions = TRUE, newMetricsOnly=TRUE)
oc.up <- update(oc.up, modRank=1, by="puFAP")
featurespace(oc.up, th=0)

hist(oc.up, predUn=predict(oc.up, x.te))
plot(oc.up$results$tprAP, oc.def$results$tpr); abline(0,1)
plot(oc.up$results$puFAP, oc.def$results$puF); abline(0,1)

hop.agg <- aggregate(holdOutPredictions(oc.up))


