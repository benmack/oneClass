context('evaluateOcc')

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

te.pred <- evList <- list()

evList.occ <- evaluateOcc(oc, te.u=te.x, te.y=te.y, positive=1, allModels=TRUE)

for (i in 1:nrow(oc$results)) {
  oc <- update(oc, modRow=i)
  te.pred[[i]] <- predict(oc, te.x)
  evList[[i]] <- dismo::evaluate(p=te.pred[[i]][te.y==1], a=te.pred[[i]][te.y!=1], 
                                 tr=evList.occ$test[[i]]@t) 
}
names(evList) <- paste("model", 1:nrow(oc$results), sep=".")

# m=4
# plot(evList.occ$test[[m]])
# plot(evList[[m]])

expect_true(identical(evList, evList.occ$test))

