### Example for predict.trainOcc

data(bananas)
### fit a model
fit <- trainOcc (x = tr[, -1], y = tr[, 1], method="bsvm", 
                       tuneGrid=expand.grid(sigma=c(0.1, 1), 
                                            cNeg=2^seq(-4, 2, 2), 
                                            cMultiplier=2^seq(4, 8, 2) ) )
# predict a raster
pred <- predict(oc, bananas$x)
plot(pred)
# register a parallel backend and predict in parallel with rasterEngine of spatial.tools
require(doParallel) # or use another parallel backend for foreach
cl <- makeCluster(detectCores()-1) # leave one core free if you don't want to go for coffee
registerDoParallel(cl)
pred <- predict(oc, bananas$x)
plot(pred)
stopCluster(cl)
