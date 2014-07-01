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
require(caret)
require(raster)
require(pROC)
require(oneClass)


# get training and test data
data(bananas)
seed <- 123456
tr.x <- bananas$tr[, -1]
tr.y <- puFactor(bananas$tr[, 1])
set.seed (seed)
te.i <- sample ( ncell (bananas$y), 1000 )
te.x <- extract (bananas$x, te.i)
te.y <- extract (bananas$y, te.i)
# default oneClass
oc <- oneClass(x=tr.x, y=puFactor(tr.y), method="ocsvm")
# customize via trainControl
set.seed(seed)
indexTr <- createFoldsPu( tr.y, k=5, index.indep = which(tr.y=='un') )
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
                          allowParallel=TRUE)

stopCluster(cl)
cl <- makeCluster(3)
registerDoParallel(cl)

### ONECLASS
### in parallel tpr/puPpv puF and negD01 are not calculated when using costum metrics... 
oc1 <- oneClass(x=tr.x, y=tr.y, method="ocsvm", 
                tuneGrid=getModelInfoOneClass()$ocsvm$grid()[1:20,], 
                trControl=trControl) #, preProcess=c("center", "scale"))
oc1

### ONECLASS
### in parallel tpr/puPpv puF and negD01 are not calculated when using costum metrics... 
puSummary <- function(data, lev = NULL, model = NULL) { # , metrics=c("puAuc", "puF", "Tpr", "puPpv")
  
  #if (nrow(data)>11)
  #  browser()
  
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  browser()
  require(pROC)
  rocObject <- try(pROC::roc(response=data$obs, predictor=data[, 'pos'], 
                             levels=c('pos', 'un')), silent = TRUE)
  puAuc <- ifelse (class(rocObject)[1] == "try-error", 0, rocObject$auc)
  
  tpr <- sum(data$pred=="pos" & data$obs=="pos")/sum(data$obs=="pos")
  puPpv <- (sum(data$pred=="pos")/length(data$pred))
  puF <- (tpr^2)/puPpv
  if (is.na(puF))
    puF[1] <- 0
  puF <- puF[[1]]
  
  negD01 <- .negD01(tpr=tpr, puPpv=puPpv)
  
  out <- c(puAuc=puAuc, puF=puF, negD01=negD01, Tpr=tpr, puPpv=puPpv) #, "SensPu", "SpecPu")
  
  return(out)
}


tr.ocsvm <- train(x=tr.x, y=tr.y,
                  method=getModelInfoOneClass()$ocsvm, 
                  trControl=trainControl(summaryFunction=puSummary))
tr.ocsvm




###
trControl$allowParallel=FALSE
oc2 <- oneClass(x=tr.x, y=tr.y, method="ocsvm", 
               tuneGrid=getModelInfoOneClass()$ocsvm$grid(), 
               trControl=trControl) #, preProcess=c("center", "scale"))
oc2


###
oc1up <- update(oc1, newMetric=puSummary, newMetricsOnly=TRUE)
oc1up$results==oc2$results




# for (i in 1:nrow(oc$resample))
#     hop <- holdOutPredictions(oc, modRow = m, partition = p)
#     pu <- c(hop$pos, hop$un[[1]])
#     data <- data.frame(obs=puFactor(rep(c(1, 0), c(length(hop$pos), length(hop$un[[1]]) ) ) ), 
#                        pred=puFactor(pu>0, positive=TRUE),
#                        pos=pu)
#   
#     resample <- rbind( resample, puSummary(data) )
# }
# oc$resample[,1:4]==resample
# head(oc$resample[,1:4])
# head(resample)

