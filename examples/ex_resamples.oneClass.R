### Example for resamples

data(bananas)
oc <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], 
               tuneGrid=expand.grid(sigma=c(0.1,1), ### not so large grid
                                    cNeg=2^seq(-5, 10, 3), 
                                    cMultiplier=2^seq(4, 15, 2)))
# visualize resamples of the ten models with highest (mean) puAuc values
resamps <- resamples(oc, modRank=1:10, metric="puAuc")
bwplot (resamps, metric='puF')