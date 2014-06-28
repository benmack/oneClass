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
### resamples
resamps <- resamples(oc, modRank=1:10, metric="puAuc")
bwplot (resamps, metric='puF')


### ----------------------------------------------------------------------------
### feature space plot
featurespace(oc.u, th=0)
