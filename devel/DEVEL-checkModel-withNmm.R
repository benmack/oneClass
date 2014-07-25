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
  tsts <- rbind(tsts, oneClass:::checkNmm_LooPu(oc, nmm.all[[m]], modRow=m,
                 fnamePlot=paste("devel/diagPlots_checkNmm_LooPu/modRow-", 
                                 m, ".pdf", sep="" ) ))



head(tsts)
alphas <- list(p.kruskal=.00001, 
               p.z=.75, 
               p.chi=.75)
valid <- cbind(p.kruskal=tsts$p.kruskal<alphas$p.kruskal, 
               p.z=tsts$p.z>alphas$p.z, 
               p.chi=tsts$p.chi>alphas$p.z)
valid <- cbind(allT=apply(valid, 1, all), valid)
summary(valid)

tsts.eval <- cbind(valid, tsts)
tsts.eval[order(tsts.eval[,1]),]

### combine ranks in order to select the model

ranks <- data.frame(kruskal=numeric(nModels),
                    z=numeric(nModels), 
                    chi=numeric(nModels))
ranks$kruskal[order(tsts$p.kruskal)] <- 1:nModels # smaller is better
ranks$z[order(-tsts$p.z)] <- 1:nModels # larger is better
ranks$chi[order(-tsts$p.chi)] <- 1:nModels # larger is better
ranks$sum <- rowSums(ranks)

### rank, reject, rank, (-;



rankModels <- function(x, ) {
  tm <- "average"
  # cbind(tsts$olap, rank(tsts$olap, ties.method=tm))
  ranks <- data.frame(kruskal=rank(x$p.kruskal, ties.method=tm),
                      z=rank(-x$p.z, ties.method=tm), 
                      chi=rank(-x$p.chi, ties.method=tm), 
                      olap=rank(x$olap, ties.method=tm))
  ranks <- cbind(ranks, sum=rowSums(ranks))
  cbind(ranks, order(ranks$sum))
  
  ranks$order <- order(ranks$sum)
  ranks$order[ranks$order] <- 1:nModels
}


i <- 0
rankedRows <- order(ranks$sum)

### investigate the models following the ranks
graphics.off()
x11()
x11()

i <- i+1
modRow <- rankedRows[i]

dev.set(2)
checkNmm_LooPu(oc, nmm.all[[modRow]], modRow)
dev.set(3)
featurespace(update(oc, modRow=modRow))





# modRow <- 30
# x <- update(oc, modRow=modRow)
# nmm <- nmm.all[[modRow]]
# aggregateHop <- TRUE




