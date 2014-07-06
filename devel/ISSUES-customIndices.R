### issue: when custom indidces are used and passed to trainOcc/train the 
### performance metrics are not calculated properly in parallel mode. 
### However, the resampling is performed properly. 
### This can be checked by extracting the hold-out predictions.
### If the resampling is computationally very expensive and is required to be run 
### in parallel a work-around would be to first run trainOcc in parallel mode and 
### store all hold-out predictions (trainControl argument savePredictions=TRUE)
### Then update the trainOcc object with new (aka the old) metrics:
### oc <- update(oc, newMetric=puSummary, newMetricsOnly=TRUE)


### tpr/ppp, puF, of puSummary does not work in model selection! 

require(doParallel)
require(raster)
require(trainOcc)
#require(caret)
#require(pROC)


# get training and test data
data(bananas)
seed <- 123456
tr.x <- bananas$tr[, -1]
tr.y <- puFactor(bananas$tr[, 1])
set.seed (seed)
te.i <- sample ( ncell (bananas$y), 1000 )
te.x <- extract (bananas$x, te.i)
te.y <- extract (bananas$y, te.i)
# default trainOcc works fine
oc <- trainOcc(x=tr.x, y=puFactor(tr.y), method="ocsvm")
plot_PPPvsTPR(oc, metric="negD01", xlim=c(0,1), ylim=c(0,1))
# customize via trainControl
set.seed(seed)
indexTr <- createFoldsPu( tr.y, k=5, index.indep = which(tr.y=='un') )
indexTe <- lapply(indexTr, function(i) which(!(1:length(tr.y))%in%i) )
sapply(indexTr, length)
sapply(indexTe, function(i) table(tr.y[i]))

#indexTr <- createFoldsPu( tr.y, k=5, index.indep = 400:500 )
#indexTe <- lapply(indexTr, function(i) which(!(1:length(tr.y))%in%i) )

cntrl <- trainControl(method="custom",
                      index=indexTr,
                      summaryFunction = puSummary,  # PU-performance metrics
                      classProbs=TRUE,              # important 
                      savePredictions = TRUE,       # important
                      returnResamp = 'all',         # for resamples.train 
                      allowParallel=TRUE)

try(stopCluster(cl))
cl <- makeCluster(3)
registerDoParallel(cl)

### trainOcc
modelInfo <- getModelInfoOneClass()$ocsvm # bsvm ok, 
### in parallel tpr/ppp puF and negD01 are not calculated when using costum metrics... 
rm(oc1)
oc1 <- trainOcc(x=tr.x, y=tr.y, method="ocsvm", 
                trControl=cntrl) #, preProcess=c("center", "scale"))
oc1
head(oc1$pred) 
holdOutPredictions(oc1) 




modelInfo <- getModelInfoOneClass()$biasedsvm # ok
modelInfo <- getModelInfoOneClass()$ocsvm

tr.oc <- train(x=tr.x, y=tr.y,
                  method=modelInfo, #getModelInfoOneClass()$ocsvm, 
                  trControl=trainControl(method="cv", 
                            summaryFunction = puSummary,  # PU-performance metrics
                            classProbs=TRUE,              # important 
                            savePredictions = TRUE,       # important
                            returnResamp = 'all',         # for resamples.train 
                            allowParallel=TRUE),
               tuneGrid=modelInfo$grid()[1:20, ],
               metric="puAuc")
head(tr.oc$pred)



### this works
trControl$allowParallel=FALSE
oc2 <- trainOcc(x=tr.x, y=tr.y, method="ocsvm", 
               tuneGrid=getModelInfoOneClass()$ocsvm$grid(), 
               trControl=trControl) #, preProcess=c("center", "scale"))
oc2

### this works
trControl$allowParallel=TRUE
trControl$index=TRUE
trControl$indexOut=TRUE

oc2 <- trainOcc(x=tr.x, y=tr.y, method="ocsvm", 
               tuneGrid=getModelInfoOneClass()$ocsvm$grid(), 
               trControl=trControl) #, preProcess=c("center", "scale"))
oc2


