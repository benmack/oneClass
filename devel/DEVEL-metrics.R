require(caret)
require(raster)
require(pROC)
require(trainOcc)


# get training and test data
data(bananas)
seed <- 123456
tr.x <- bananas$tr[, -1]
tr.y <- puFactor(bananas$tr[, 1])
set.seed (seed)
te.i <- sample ( ncell (bananas$y), 1000 )
te.x <- extract (bananas$x, te.i)
te.y <- extract (bananas$y, te.i)
# ocsvm
oc <- trainOcc(x=tr.x, y=puFactor(tr.y), method="ocsvm")
plot_PPPvsTPR(oc, metric="negD01", ylim=c(0,1), clim=c(0,1))
# biasedsvm
oc <- trainOcc(x=tr.x, y=puFactor(tr.y))
plot_PPPvsTPR(oc, metric="negD01")

plot_PPPvsTPR(oc, metric="negD01", identifyPoints=TRUE)


# customize via trainControl
set.seed(seed)
indexTr <- createMultiFoldsPu( tr.y, k=5, times=10, index.indep = which(tr.y=='un') )
indexTe <- lapply(indexTr, function(i) which(!(1:length(tr.y))%in%i) )
sapply(indexTr, length)
sapply(indexTe, function(i) table(tr.y[i]))

trControl <- trainControl(method='custom', 
                          index=indexTr,
                          indexOut=indexTe, 
                          summaryFunction = puSummary,  # PU-performance metrics
                          classProbs=TRUE,              # important 
                          savePredictions = TRUE,       # important
                          returnResamp = 'all',         # for resamples.train 
                          allowParallel=FALSE)
tuneGrid <- expand.grid(sigma=c(seq(.001, .01, .001),   #c(.001, .005, .0075, .01, .025, .05), 
                                seq(.01, .1, .01)[-1], 
                                seq(.1, .5, .1)[-1]), # c(.025, .05, .075), 
                        nu=c(.01, .1, .2, .3, .4, .5))
oc <- trainOcc(x=tr.x, y=tr.y, method="ocsvm", 
               trControl=trControl, 
               tuneGrid=tuneGrid)
plot_PPPvsTPR(oc, metric="puF")

################################################################################
### create a new summary metrics
negD01 <- -sqrt( (1-oc$results$tpr)^2 + oc$results$ppp^2 )
plot_PPPvsTPR(oc, metric=negD01)


