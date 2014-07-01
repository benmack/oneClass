### issue: when custom indidces are used and passed to oneClass/train the 
### performance metrics are not calculated properly in parallel mode. 
### However, the resampling is performed properly. 
### This can be checked by extracting the hold-out predictions.
### If the resampling is computationally very expensive and is required to be run 
### in parallel a work-around would be to first run oneClass in parallel mode and 
### store all hold-out predictions (trainControl argument savePredictions=TRUE)
### Then update the oneClass object with new (aka the old) metrics:
### oc <- update(oc, newMetric=puSummary, newMetricsOnly=TRUE)


### Tpr/puPpv, puF, of puSummary does not work in model selection! 

require(doParallel)
#require(caret)
#require(raster)
#require(pROC)
#require(oneClass)


# get training and test data
data(bananas)
seed <- 123456
tr.x <- bananas$tr[, -1]
tr.y <- puFactor(bananas$tr[, 1])
set.seed (seed)
te.i <- sample ( ncell (bananas$y), 1000 )
te.x <- extract (bananas$x, te.i)
te.y <- extract (bananas$y, te.i)
# default oneClass works fine
oc <- oneClass(x=tr.x, y=puFactor(tr.y), method="ocsvm")
plot_puPpvVsTpr(oc, metric="negD01", xlim=c(0,1), ylim=c(0,1))
# customize via trainControl
set.seed(seed)
indexTr <- createFoldsPu( tr.y, k=5, index.indep = which(tr.y=='un') )
indexTe <- lapply(indexTr, function(i) which(!(1:length(tr.y))%in%i) )
sapply(indexTr, length)
sapply(indexTe, function(i) table(tr.y[i]))

indexTr <- createFoldsPu( tr.y, k=5, index.indep = 400:500 )
indexTe <- lapply(indexTr, function(i) which(!(1:length(tr.y))%in%i) )

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

### ONECLASS
modelInfo <- getModelInfoOneClass()$ocsvm # bsvm ok, 
### in parallel tpr/puPpv puF and negD01 are not calculated when using costum metrics... 
oc1 <- oneClass(x=tr.x, y=tr.y, method=modelInfo, 
                tuneGrid=modelInfo$grid()[1:20,], 
                trControl=cntrl) #, preProcess=c("center", "scale"))
oc1
head(oc1$pred) 




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
oc2 <- oneClass(x=tr.x, y=tr.y, method="ocsvm", 
               tuneGrid=getModelInfoOneClass()$ocsvm$grid(), 
               trControl=trControl) #, preProcess=c("center", "scale"))
oc2

### this works
trControl$allowParallel=TRUE
trControl$index=TRUE
trControl$indexOut=TRUE

oc2 <- oneClass(x=tr.x, y=tr.y, method="ocsvm", 
               tuneGrid=getModelInfoOneClass()$ocsvm$grid(), 
               trControl=trControl) #, preProcess=c("center", "scale"))
oc2


