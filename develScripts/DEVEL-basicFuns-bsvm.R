context('Basic functionalities with bsvm')

require(devtools)
# load_all(pkg='D:/github/rasterTiled/')
data(bananas)
u <- bananas$x

seed <- 123456
tr.x <- bananas$tr[, -1]
tr.y <- bananas$tr[, 1]

set.seed (seed)
te.i <- sample ( ncell (bananas$y), 1000)
te.x <- extract (bananas$x, te.i)
te.y <- extract (bananas$y, te.i)

### ----------------------------------------------------------------------------
### set up a small grid and get the model
set.seed (seed)
index <- createResamplePu ( tr.y, times = 25, index.indep = 276:500)
trControl <- trainControl (index=index,
                           summaryFunction = puSummary, # PU-performance metrics
                           classProbs=TRUE, # important
                           savePredictions = TRUE, # important
                           returnResamp = 'all') # important (by now?) for resamples.train
tuneGrid = expand.grid(sigma=c(.1, 1),  # BSVM parameters
                       cNeg=2^seq(-3, 15, 3), 
                       cMultiplier=2^seq(2, 8, 2) )
oc <- oneClass ( x = tr.x, y = tr.y, tuneGrid=tuneGrid, trControl = trControl )
plot(oc, plotType='level')

### ----------------------------------------------------------------------------
### predict new data
# predict with raster method 
predBin.r <- predict(u, oc)
plot(predBin.r)
pred.r <- predict(u, oc, type="prob")
plot(pred.r)
# predict with oneClass method -> returns decision values
pred.r <- predict(oc, u)
plot(pred.r)

### ----------------------------------------------------------------------------
### extract hold-out predictions and aggregate them
holdOutPredictions(oc) # requires modelPosition, sort, ...
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
oc.up <- update(oc, modRank=5)
pred.up <- predict(oc.up, u)
hist(oc.u, pred.up)

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
featurespace(oc.u, th=0)
