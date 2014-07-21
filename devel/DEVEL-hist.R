data(bananas)

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


plot_PPPvsTPR_thDep <- function (x) {
  
  
}