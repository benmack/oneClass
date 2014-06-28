### Example for evaluateOneClass

# get training and test data
data(bananas)
seed <- 123456
tr.x <- bananas$tr[, -1]
tr.y <- bananas$tr[, 1]
set.seed (seed)
te.i <- sample ( ncell (bananas$y), 1000 )
te.x <- extract (bananas$x, te.i)
te.y <- extract (bananas$y, te.i)
# run oneClass 
oc <- oneClass(x=tr.x, y=puFactor(tr.y), 
               tuneGrid=expand.grid(sigma=c(0.1,1), ### not so large grid
                                    cNeg=2^seq(-5, 10, 3), 
                                    cMultiplier=2^seq(4, 15, 2)))
# evaluate the final model
ev <- evaluateOneClass(oc, y=te.y, u=te.x)
# besides the thresholds used, this is identical to: 
te.pred <- predict(oc, te.x)
ev <- evaluate(p=te.pred[te.y==1], a=te.pred[te.y!=1])
# evaluate several models
# e.g. evaluate models with a true positive rate (Tpr) higher than 0.8 and a 
# positive prediction probability (puPpv) small than 0.4
modRows <- which(oc$results$Tpr>=0.8 & oc$results$puPpv<0.4)
ev <- evaluateOneClass(oc, y=te.y, u=te.x, modRow=modRows)
# plot the pu-performance metric versus the maximum kappa 
evList <- print(ev)
plot(evList$puF, evList$mxK.K, xlab="puF", ylab="max. Kappa")
