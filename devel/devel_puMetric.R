require(doParallel)
try(stopCluster(cl))
cl <- makeCluster(detectCores())
registerDoParallel(cl)

data(bananas)
tuneGrid <- expand.grid(sigma=c(1:10)*rep(c(.1, 1), each=10), 
                        nu=c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25))
model <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], 
                  method="ocsvm", tuneGrid=tuneGrid)
pairs(model$results[, c("puF1", "puF")])
model <- update(model, aggregatePredictions=TRUE, 
                metric="puFAP")
model <- update(model, aggregatePredictions=TRUE, 
                puSummaryFunction = puSummaryThLoop, 
                metric="th.puFAP")

featurespace(model)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
U <- bananas$x
folder_out="D:/github/devel_puMetric"
dir.create(folder_out)

### plot histograms with 


# 
# 
# pairs(model$results[, c("puF", "puFAP", "thPuFAP")])
# pairs(model$results[, c("puF1", "puF1AP", "thPuF1AP")])
# pairs(model$results[, c("puF", "puF1")])
# pairs(model$results[, c("puFAP", "puF1AP")])
# pairs(model$results[, c("thPuFAP", "thPuF1AP")])
# 
# mp.puF <- modelPosition(model, modRank=1, by="puF")
# mp.puFAP <- modelPosition(model, modRank=1, by="puFAP")
# mp.thPuFAP <- modelPosition(model, modRank=1, by="thPuFAP")
# mp.puF1 <- modelPosition(model, modRank=1, by="puF1")
# mp.puF1AP <- modelPosition(model, modRank=1, by="puF1AP")
# mp.thPuF1AP <- modelPosition(model, modRank=1, by="thPuF1AP")
# 
# featurespace(update(model, modRow=mp.puF$row))
# featurespace(update(model, modRow=mp.puFAP$row))
# featurespace(update(model, modRow=mp.thPuFAP$row))
# featurespace(update(model, modRow=mp.puF1$row))
# featurespace(update(model, modRow=mp.puF1AP$row))
# featurespace(update(model, modRow=mp.thPuF1AP$row))
# pred.puF <- predict(update(model, modRow=mp.puF$row), 
#                     bananas$x, returnRaster=FALSE)
# pred.puFAP <- predict(update(model, modRow=mp.puFAP$row), 
#                       bananas$x, returnRaster=FALSE)
# pred.thPuFAP <- predict(update(model, modRow=mp.thPuFAP$row), 
#                         bananas$x, returnRaster=FALSE)
# hist(update(model, modRow=mp.puF$row), pred.puF)
# hist(update(model, modRow=mp.puFAP$row), pred.puFAP)
# hist(update(model, modRow=mp.thPuFAP$row), pred.thPuFAP)
# abline(v=c(0, model$results[mp.thPuFAP$row, "thAP"] ))