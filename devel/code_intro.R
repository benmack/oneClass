require(devtools)
load_all(".", quiet = TRUE)
require(raster)

# ----
# data
data(bananas)
seed <- 123456
tr.x <- bananas$tr[, -1]
tr.y <- puFactor(bananas$tr[, 1], positive=1)
set.seed(seed)
tr.index <- createFolds(tr.y, k=10, returnTrain=TRUE)
set.seed(seed)
te.i <- sample(ncell(bananas$y), 1000)
te.x <- extract(bananas$x, te.i)
te.y <- extract(bananas$y, te.i)

# ----
# train
ocsvm.fit <- trainOcc(x=tr.x, y=tr.y, method="ocsvm", index=tr.index)
# ----
# where does the warning come from?
# Warning message:
# In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
#   There were missing values in resampled performance measures.
ocsvm.fit$resample
# => SOLVED 

# ----
# Warning appears when predicting a raster in parallel
# Warning message:
# In .local(x, ...): min value not known, use setMinMaxWarning message:
# In .local(x, ...): max value not known, use setMinMaxWarning message:
# In .local(x, ...): min value not known, use setMinMax
require(doParallel)
cl <- makeCluster(detectCores()-1)
doParallel:::registerDoParallel(cl)
ocsvm.pred <- predict(ocsvm.fit, bananas$x)
ocsvm.bin <- ocsvm.pred>0
# => SOLVED

# ----
# 
hist(ocsvm.fit, ocsvm.pred, th=0)
plot(ocsvm.pred, col=brewer.pal(9, "RdBu"))
plot(ocsvm.pred>0, col=brewer.pal(9, "RdBu")[c(2,9)])

