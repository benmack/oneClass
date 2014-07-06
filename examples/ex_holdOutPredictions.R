### Example for holdOutPredictions

data(bananas)
oc <- trainOcc(x = bananas$tr[, -1], y = bananas$tr[, 1], 
               tuneGrid=expand.grid(sigma=c(.1, 1, 10), 
                                     cNeg=0.0625, 
                                     cMultiplier=64))
### hold-out predictions of the selected model
hop <- holdOutPredictions(oc)
### extract the hold-out samples of a model corresponding to 
### a aspecific row of the results table, returned when 
### printing a \'trainOcc\' object. 
hop <- holdOutPredictions(oc, modRow=3)
### extract the hold-out predictions of a partition
hop <- holdOutPredictions(oc, partition=3) 