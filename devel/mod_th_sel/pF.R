require(rPref)
require(dplyr)
require(devtools)
load_all(".", quiet = TRUE)
require(raster)

# ----
# data
data(bananas)
trs <- bananas_trainset(nP=50, nU=200)
plot(trs[, -1], pch=c(1, 3)[as.numeric(trs$y)],
     col=c("darkred", "darkblue")[as.numeric(trs$y)])

# ----
# train
fit <- trainOcc(x=trs[, -1], y=trs$y, index=tr.index)
featurespace(fit, thresholds = 0)
hist(fit)

# ----
# Explore the pareto front over all models and thresholds
# the two conflicting goals of a high true positive rate and
# a high probabilty of positive prediction 

idx <- group_indices_(
  fit$pred, 
  .dots=as.character(fit$modelInfo$parameters$parameter))
u_idx <- sort(unique(idx))

registerDoParallel(cores=4)
res <- foreach(i = u_idx, .packages="oneClass") %dopar% 
  cbind(id=i, puSummaryThLoop(fit$pred[idx==i, ], returnAll=TRUE))
res <- do.call(rbind, res)
str(res)

candidates <- psel(res, high(tpr) * low(ppp)) 
str(candidates)

ggplot(res, aes(x = ppp, y = tpr)) + 
  geom_point(shape = 21) + 
  geom_point(data=candidates, size = 3, col="blue") +
  geom_point(data=fit$results, size = 3, col="red")
