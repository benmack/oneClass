#' Title
#'
#' @param results 
#' @param metric 
#' @param control_method 
#'
#' @return
#' @export
#'
#' @examples
fake_trainOcc_bsvm <- function(results, metric=NULL, 
                               control_method="none") {

  method="biasedsvm"
  data(bananas)

  # >>> Method-dependent! 
  tuneGrid <- expand.grid(sigma=1,
                          cNeg=1,
                          cMultiplier=1)
  # <<<
  
  model <- trainOcc(x=bananas$tr[1:41, -1], y=bananas$tr[1:41, 1], 
                    method=method, tuneGrid=tuneGrid, verbose=F)
  model$pred <- NULL
  model$trainingData <- NULL
  
  model$results <- results
  model$results <- cbind(x$results[, names(x$results)%in%names(x$bestTune)],
                         x$results[, !names(x$results)%in%names(x$bestTune)])
  
  if (!is.null(metric))
    model$metric <- metric
  
  mp <- modelPosition(model, by=model$metric)
  model$bestTune <- mp$param
  model$control$method = control_method

  model
}
