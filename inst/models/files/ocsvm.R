modelInfo <- 
  list(label="one-class svm", 
       library="kernlab",
       loop=NULL,
       type="Classification",
       parameters=data.frame(parameter = c("sigma", "nu"), 
                               class = rep("numeric", 2),
                               label = c("Sigma", "Nu")),
       grid = function(x, y, len = NULL, seedGrid=NULL) {
         grid <- expand.grid(sigma=c(.001, .01, .1, 1, 10, 100),  # ocsvm parameters
                             nu=c(.01, .05, .1, .15, .2, .25) )
         return(grid)
       },
       fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
         ksvm(x = as.matrix(x[y=='pos', ]), y = NULL, # only positives used
              kernel = "rbfdot",
              kpar = list(sigma = param$sigma), 
              nu = param$nu, cross=0)
       }, 
       predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
         predicted <- predict(modelFit, newdata)
         predicted <- puFactor(predicted, positive=TRUE)
         
         return(predicted)
       },
       ### prob
       prob=function(modelFit, newdata, preProc = NULL, submodels = NULL) {
         probs <- predict(modelFit, newdata, type = "decision")
         probs <- cbind(probs, probs)
         colnames(probs) <- c('pos', 'un')
         return(probs)
       },
       ### predictors
       predictors=NULL,
       ### tags
       tags=NULL,
       ### levels
       levels=function(x) c("un", "pos"),
       ### sort
       sort=ocsvmSort <- function(x) x[order(-x$sigma, -x$nu), ],
       ### varImp
       varimp=NULL)  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
