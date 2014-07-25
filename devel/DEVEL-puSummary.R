require(doParallel)
require(raster)
require(oneClass)

cl <- makeCluster(detectCores()) 
registerDoParallel(cl)

data(bananas)
x.te <- bananas$x[sample(ncell(bananas$x), 10000)]

oc <- trainOcc(x = bananas$tr[, -1], y = bananas$tr[, 1])

### get the data as required for a puSummary
hop <- holdOutPredictions( oc,aggregate=TRUE)
pus <- puSummaryWithPppDepTpr(.dataForSummaryFunction(hop), lev=c('pos', 'un'))

### merge and calculate the hops
oc.up <- update(oc, newMetric=puSummaryWithPppDepTpr, 
                aggregatePredictions = TRUE, newMetricsOnly=TRUE)
featurespace(oc.up, th=0)

plot_PPPvsTPR(oc.up)

head(oc.up$results)
plot(c(100, 95, seq(90,50,-10))/100, oc.up$results[1, 9:15], 
     xlab="TPR", ylab="PPP", type="n")
for(i in 1:nrow(oc.up$results))
  lines(c(100, 95, seq(90,50,-10))/100, oc.up$results[i, 9:15], 
        xlab="TPR", ylab="PPP", lwd=2, col=rgb(0,0,0,alpha=.1))

### ----------------------------------------------------------------------------
### MODEL SELECTION
### select the model which rejects most of the unlabeled training data while 
### retaining a high true positive rate
plot(oc.up$results$pppAtTp100AP, oc.up$results$puFAP)
hist(oc.up$results$pppAtTp100AP)
plot(oc.up$results$pppAtTp100AP, oc.up$results$pppAtTp50AP)

mr <- which.min(oc.up$results$pppAtTp100AP)
oc.up <- update(oc.up, modRow=mr)
featurespace(oc.up, th=oc.up$results$thAtTp100AP[mr])

hist(oc.up, predict(oc.up, x.te))
