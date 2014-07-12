### plot_PPPvsTPR

require(oneClass)
require(devtools)

################################################################################
### method 
method <- 'biasedsvm'
################################################################################
### data 
data(bananas)
u <- bananas$x
seed <- 123456
tr.x <- bananas$tr[, -1]
tr.y <- puFactor(bananas$tr[, 1], 1)
set.seed (seed)
te.i <- sample ( ncell (bananas$y), 1000)
te.x <- extract (bananas$x, te.i)
te.y <- extract (bananas$y, te.i)

### ----------------------------------------------------------------------------
### set up a small grid and get the model
sigest(as.matrix(tr.x[tr.y=='pos', ]))
set.seed (seed)
index <- createFolds ( tr.y, k = 15, returnTrain = TRUE)
trControl <- trainControl (index=index,
                           summaryFunction = puSummary, # PU-performance metrics
                           classProbs=TRUE, # important
                           savePredictions = TRUE, # important
                           returnResamp = 'all') # important (by now?) for resamples.train
tuneGrid = list(ocsvm=expand.grid(sigma=c(.01, .025, .05, .075, .1 , .25, .5, .75, 1, 2.5, 5), 
                                  nu=c(.01, 0.05, .1, 0.15, .2)), 
                biased=expand.grid(sigma=c(.1, 1),  # BSVM parameters
                       cNeg=2^seq(-3, 15, 3), 
                       cMultiplier=2^seq(2, 8, 2) ), 
                maxent=expand.grid(beta=c(.1, .5, 1, 2, 3, 6, 9, 15) ))[[method]]
oc <- trainOcc ( x = tr.x, y = tr.y, 
                 method=method, tuneGrid=tuneGrid, trControl = trControl )
#plot(oc, plotType='level')
#plot(oc)

### ----------------------------------------------------------------------------
### plot
plot_PPPvsTPR(oc)
### ----------------------------------------------------------------------------
### identify models that you would like to investigate in more detail
### or and click on the points from which you want to know the model infor 
plot_PPPvsTPR(oc, identify=TRUE)






### ----------------------------------------------------------------------------
### predict new data
# predict with raster method 
predBin.r <- predict(u, oc)
plot(predBin.r)
pred.r <- predict(u, oc, type="prob")
plot(pred.r)
# predict with trainOcc method -> returns decision values
pred.r <- predict(oc, u)
plot(pred.r)

### ----------------------------------------------------------------------------
### extract hold-out predictions and aggregate them
hop <- holdOutPredictions(oc) # requires modelPosition, sort, ...
aggregate(hop)

### ----------------------------------------------------------------------------
### hist plot
hist(oc, pred.r)

### ----------------------------------------------------------------------------
### compare models 
oc$resample
resamps <- resamples ( oc, modRank = 1:11 )
bwplot (resamps)
bwplot (resamps, metric='puF')

### ----------------------------------------------------------------------------
### update model
source("inst/models/files/maxent.R")
oc$modelInfo <- modelInfo
oc.up <- update(oc, modRank=5)
pred.up <- predict(oc.up, u)
hist(oc.up, pred.up)

### ----------------------------------------------------------------------------
### evaluate ### !!! code: add a tag to the oc-object in order to warn if the user tests on the hold-outs
### all models   
ev <- evaluate(oc, y = te.y, u=te.x, positive=1, allModels=TRUE)
plot(ev)
### evaluate model at several thresholds   
ev <- evaluate(oc, y = te.y, u=te.x, positive=1) 
plot(ev)
### evaluate model at one threshold 
evaluate(ev, th=0)
evaluate(ev, th='max.kappa')


### ----------------------------------------------------------------------------
### feature space plot
featurespace(oc.up, th=0)
featurespace(oc, th=0)
for (i in 1:nrow(tuneGrid)) {
  featurespace(update(oc, modRow=i), th=0)
  title(paste(oc$result[i, c(1:2, 4:6)]))
}