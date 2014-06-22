
modelInfo <- 
  list(label="biasedsvm", 
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
         if (is.data.frame(newdata) | is.matrix(newdata)) {
           predicted <- predict(modelFit, newdata) 
         } else if (  class(newdata)=="RasterBrick" | 
                        class(newdata)=="RasterStack") { 
           stop("Not implemented")
           predicted <- predict(newdata, modelFit)
         } else if (class(newdata)=="ffraster") {
           stop("Not implemented")
           predicted <- predict(newdata, modelFit)
         }
         return(predicted)
       },
       ### prob
       prob=function(modelFit, newdata, preProc = NULL, submodels = NULL) {
         if (is.data.frame(newdata) | is.matrix(newdata)) {
           
           predicted <- predict(modelFit, newdata) 
           probs <- predict(modelFit, newdata, type = "decision")
           probs <- cbind(probs, probs)
           colnames(probs) <- modelFit@lev
           
         } else if (  class(newdata)=="RasterBrick" | 
                        class(newdata)=="RasterStack") { 
           stop("Not implemented")
         } else if (class(newdata)=="ffraster") {
           stop("Not implemented")
         }
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
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
