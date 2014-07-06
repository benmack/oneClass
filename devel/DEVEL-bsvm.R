context('Prediction method for trainOcc objects.')

# require(devtools)
# load_all(pkg='D:/github/rasterTiled/')
data(bananas)
u <- bananas$x

### ----------------------------------------------------------------------------
### parallel backend
require(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

### ----------------------------------------------------------------------------
### get some training data
seed <- 123456
tr.x <- bananas$tr[, -1]
tr.y <- puFactor(bananas$tr[, 1])

### ----------------------------------------------------------------------------
### the model
modelInfo <- 
  list(label="biased svm", 
       library="kernlab",
       loop=NULL,
       type="Classification",
       parameters=
         data.frame(parameter = c("sigma", "cNeg", "cMultiplier"), 
                    class = rep("numeric", 3),
                    label = c("Sigma", "Cost (U)", "Multiplier Cost (P)")),
       grid = function(x, y, len = NULL, seedGrid=NULL) {
         grid <- expand.grid(sigma=c(.1, 1, 10, 100),  
                             cNeg=2^seq(-10, 15, 3),
                             cMultiplier=2^seq(2, 15, 2) ) 
       },
       fit = function(x, y, wts, param, lev, last, weights, 
                      classProbs, ...) {
         cPos <- param$cNeg*param$cMultiplier
         ksvm(x = as.matrix(x), y = y,
              kernel = rbfdot,
              kpar = list(sigma = param$sigma),
              C = 1,
              class.weights=c("un" = param$cNeg, "pos" = cPos),
              prob.model = FALSE, #=class.probs
              ...)
       }, 
       predict = function(modelFit, newdata, preProc = NULL, 
                          submodels = NULL) {
           predicted <- predict(modelFit, newdata) 
         return(predicted)
       },
       ### prob
       prob=function(modelFit, newdata, preProc = NULL, submodels = NULL) {
           predicted <- predict(modelFit, newdata) 
           probs <- predict(modelFit, newdata, type = "decision")
           probs <- cbind(probs, probs)
           colnames(probs) <- modelFit@lev
         return(probs)
       },
       ### predictors
       predictors=NULL,
       ### tags
       tags=NULL,
       ### levels
       levels=function(x) lev(x),
       ### sort
       sort=function(x) x[order(-x$sigma, -x$cNeg, -x$cMultiplier), ],
       ### varImp
       varimp=NULL) 

### ----------------------------------------------------------------------------
### train
tuneGrid=expand.grid(sigma=c(.1, 1),  # BSVM parameters
                       cNeg=2^seq(-3, 15, 3), 
                       cMultiplier=2^seq(2, 8, 2) )
tr <- train(x=tr.x, y=tr.y, method=modelInfo, tuneGrid=tuneGrid, 
            trControl=trainControl(summaryFunction=puSummary))



