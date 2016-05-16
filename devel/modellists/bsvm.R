method <- 'biasedsvm'
# ----
# data 
data(bananas)
tr.x <- bananas$tr[, -1]
tr.y <- puFactor(bananas$tr[, 1], 1)
set.seed(1)

# ----
# run the default
oc <- trainOcc(x=tr.x, y=tr.y)

### change something in inst/models/ocsvm and run the following two lines to get the changes
source("inst/models/parseModels.R")
load_all()

oc <- trainOcc(x=tr.x, y=tr.y)

plot(oc, plotType='level')

featurespace(update(oc, modRow=2), th=0)
featurespace(update(oc, modRow=8), th=0)

expect_true(oc$results$tpr[2]>oc$results$tpr[8])

