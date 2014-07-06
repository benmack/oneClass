context('Test OCSVM')

# require(devtools)
# require(testthat)

################################################################################
### method 
method <- 'ocsvm'
################################################################################
### data 
dt <- threeGaussians()
u <- dt$te[,-1]
seed <- 123456
tr.x <- dt$tr[, -1]
tr.y <- puFactor(dt$tr[, 1], 1)
set.seed (seed)
te.x <- dt$te[,-1]
te.y <- dt$te[,1]

## ----------------------------------------------------------------------------
### set up a small grid which leads to a over and underfitted 
# sigest(as.matrix(tr.x[tr.y=='pos', ]))
tuneGrid = ocsvm=expand.grid(sigma=c(.01, 10), 
                                  nu=c(.1, .5))

### change something in inst/models/ocsvm and run the following two lines to get the changes
### source("inst/models/parseModels.R")
### load_all()

oc <- trainOcc ( x = tr.x, y = tr.y, 
                 method=method, tuneGrid=tuneGrid)
#plot(oc, plotType='level')
#plot(oc)

expect_true(oc$results$tpr[1]>oc$results$tpr[4])

