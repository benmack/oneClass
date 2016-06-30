rm(list=ls())
require(doParallel)
require(raster)
require(oneClass)
require(mclust)

cl <- makeCluster(detectCores()) 
registerDoParallel(cl)

data(bananas)
tr <- bananas$tr

set.seed(123)
te.idx <- sample(ncell(bananas$x), 2000)
te.y <- bananas$y[te.idx]; table(te.y)
te.x <- bananas$x[te.idx]

set.seed(123)
index <-  createFoldsPu(tr[,1], k=20, index.indep=271:520)
method <- "biasedsvm"
oc <- trainOcc (x = tr[, -1], y = tr[, 1], index=index) #,tuneGrid=getModelInfoOneClass("biasedsvm")$biasedsvm$grid()[c(1,2,17,22,29,37,79,99,142,169),])
# id <- plot_PPPvsTPR(oc, identify=TRUE)
oc <- update(oc, newMetric = puSummary, aggregate=TRUE)
oc$metric <- "puFAP"
oc <- update(oc, modRank=1)

te.pred.all <- foreach(m=1:nrow(oc$results)) %dopar%
  predict(update(oc, modRow=m), te.x)

nmm.all <- foreach(m=1:nrow(oc$results), .packages="mclust") %dopar%
  densityMclust(te.pred.all[[m]])

tsts <- c()
for (m in 1:nrow(oc$results))
  tsts <- rbind(tsts, nmm_selection_criteria(oc, nmm=nmm.all[[m]], modRow=m,
                 fnamePlot=paste("devel/diagPlots_checkNmm_LooPu/modRow-", 
                                 m, ".pdf", sep="" ) ))


ranking <- nmm_rank_models_iteratively(tsts)

rankedRows <- order(ranking$orderIter)

### investigate the models following the ranks
i <- 0


i <- i+1
modRow <- rankedRows[i]; modRow

# dev.set(2)
# checkNmm_LooPu(oc, nmm.all[[modRow]], modRow)
# dev.set(3)
featurespace(update(oc, modRow=modRow))

# modRow <- 30
# x <- update(oc, modRow=modRow)
# nmm <- nmm.all[[modRow]]
# aggregateHop <- TRUE
