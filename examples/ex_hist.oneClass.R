### Example for hist.trainOcc

data(bananas)
### an underfitted model 
oc <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], 
                tuneGrid=expand.grid(sigma=0.1, 
                                     cNeg=0.5, 
                                     cMultiplier=16))
### predict 10% or the unlabeled data and plot 
  # the diagnostic distributions plot
  # and the model in the 2D feature space 
set.seed(123)
idx.pred <- sample(400*400, 16000)
hist(oc, predict(oc, bananas$x[][idx.pred,]), th=0)
featurespace(oc, th=0)

### an overfitted model 
oc <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], 
                tuneGrid=expand.grid(sigma=1, 
                                     cNeg=32, 
                                     cMultiplier=16))
### predict 10% or the unlabeled data and plot 
# the diagnostic distributions plot
# and the model in the 2D feature space 
set.seed(123)
idx.pred <- sample(400*400, 16000)
hist(oc, predict(oc, bananas$x[][idx.pred,]), th=0)
featurespace(oc, th=0)

### a good model 
oc <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], 
                tuneGrid=expand.grid(sigma=1, 
                                     cNeg=0.0625, 
                                     cMultiplier=64))
### predict 10% or the unlabeled data and plot 
# the diagnostic distributions plot
# and the model in the 2D feature space 
set.seed(123)
idx.pred <- sample(400*400, 16000)
hist(oc, predict(oc, bananas$x[][idx.pred,]), th=0)
featurespace(oc, th=0)
