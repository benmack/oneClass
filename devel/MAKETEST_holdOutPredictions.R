context('Test hold-out predictions')

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

set.seed(seed)
tuneGrid = ocsvm=expand.grid(sigma=c(.01, 10), 
                             nu=c(.1, .5))



## ----------------------------------------------------------------------------
### test cv
set.seed(seed)
index <- createFolds(tr.y, k=5, returnTrain=TRUE)

oc <- trainOcc ( x = tr.x, y = tr.y, method=method,
                 tuneGrid=tuneGrid, index=index)
hop <- holdOutPredictions(oc)

### compare hop with manually calculated hops
part <- 1
idx.tr <- index[[part]]
idx.te <- which(!(1:length(tr.y))%in%idx.tr)
hop.vect <- c(hop$pos, hop$un)
hop.vect <- hop.vect[match(idx.te, as.numeric(names(hop.vect)))]

param <- modelPosition(oc)$param
oc.part <- trainOcc ( x = tr.x[idx.tr,], y = tr.y[idx.tr], method=method,
                      tuneGrid=param)
hop.man <- predict(oc.part, tr.x[idx.te,])

hop.man==hop.vect




## ----------------------------------------------------------------------------
### test loo-pu
table(tr.y)

set.seed(seed)
index <- createFoldsPu(tr.y, k=20, index.indep=21:70)
table(tr.y[index[[1]]])

oc <- trainOcc ( x = tr.x, y = tr.y, method=method,
                 tuneGrid=tuneGrid, index=index)
hop <- holdOutPredictions(oc)

### compare hop with manually calculated hops
part <- 1
idx.tr <- index[[part]]
idx.te <- which(!(1:length(tr.y))%in%idx.tr)
hop.vect <- c(hop$pos[1], hop$un[[1]])
hop.vect <- hop.vect[match(idx.te, as.numeric(names(hop.vect)))]

param <- modelPosition(oc)$param
oc.part <- trainOcc ( x = tr.x[idx.tr,], y = tr.y[idx.tr], method=method,
                      tuneGrid=param)
hop.man <- predict(oc.part, tr.x[idx.te,])

hop.man==hop.vect
