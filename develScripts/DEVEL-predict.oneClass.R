context('Prediction method for oneClass objects.')

# require(devtools)
# load_all(pkg='D:/github/rasterTiled/')
require(spatial.tools)

### ----------------------------------------------------------------------------
### parallel backend
require(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

### ----------------------------------------------------------------------------
### get some data a mask and a training set
data(bananas)
mask <- bananas$y*0+1
mask[50:300, 100:300] <- NA
plot(mask)
u <- bananas$x
seed <- 123456
tr.x <- bananas$tr[, -1]
tr.y <- bananas$tr[, 1]

### ----------------------------------------------------------------------------
### set up a small grid and train the model
set.seed (seed)
index <- createFolds ( puFactor(tr.y), k = 5 )
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
### prediction of a data frame
pred <- predict(oc, bananas$x[])
### ----------------------------------------------------------------------------
### prediction of a raster object 
### if package spatial.tools is available, allowParallel=TRUE, 
### and a parallel backend registered, prediction is done in parallel 
pred <- predict(oc, bananas$x)
plot(pred)
### ----------------------------------------------------------------------------
### prediction of a raster object with a mask (this saves prediction, but 
### not loading the data from the file)
pred <- predict(oc, bananas$x, mask=mask)
plot(pred)
### ----------------------------------------------------------------------------
### tiledRaster
### ...
require(rasterTiled)
ut <- rasterTiled(bananas$x, nTiles=4)
utm <- rasterTiled(bananas$x, mask=mask, nTiles=4)
pred <- predict(ut, oc, allowParallel=TRUE)
pred <- predict(utm, oc, allowParallel=TRUE)
plot(pred)


### ----------------------------------------------------------------------------
### compare prediction times
### ...
system.time(pred <- predict(oc, bananas$x))
system.time(pred <- predict(oc, bananas$x, mask=mask))
system.time(pred <- predict(ut, oc, allowParallel=TRUE))
system.time(pred <- predict(utm, oc, allowParallel=TRUE))
