################################################################################
#' @title Predict method for \code{\link{trainOcc}} objects.
#'
#' @description The prediction works on usual matrices/data frames, the \code{raster*} 
#' and \code{rasterTiled} objects. If a parallel backend for \code{foreach} (e.g. via \code{doParallel})
#' is used for the prediction of \code{raster*} and \code{rasterTiled} objects. Small data sets are 
#' are processed faster sequentially due to the parallel overhead.
#' 
#' @note \code{type='probs'} does NOT return probabilities but the continuous 
#' decision values of the classifier, e.g. distances in the case of the one-class 
#' and biased svm) and not probabilities!
#' 
#' @param object a \code{\link{trainOcc}} object 
#' @param newdata new data to predict on. 
#' @param type default is 'prob' which however returns the continuous decision values (see note!)
#' @param allowParallel should parallel processing be allowed. 
#' @param returnRaster default \code{TRUE}
#' @param mask if given and if \code{returnRaster=TRUE} only predictions of the valid cells are returned.
#' @param ... other arguments that can be passed to \code{\link{predict.train}}.
#' @return the predicted data, eventually returned as \code{rasterLayer}.
#' @method predict trainOcc
#' @examples
#' \dontrun{
#' data(bananas)
#' ### fit a model
#' fit <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], method="biasedsvm", 
#'                  tuneGrid=expand.grid(sigma=c(0.1, 1), 
#'                                       cNeg=2^seq(-4, 2, 2), 
#'                                       cMultiplier=2^seq(4, 8, 2) ) )
#' # predict a raster
#' pred <- predict(fit, bananas$x)
#' plot(pred)
#' 
#' # register a parallel backend and predict in parallel with rasterEngine of spatial.tools
#' require(doParallel) # or use another parallel backend for foreach
#' cl <- makeCluster(detectCores()-1) # leave one core free if you don't want to go for coffee
#' registerDoParallel(cl)
#' pred <- predict(fit, bananas$x)
#' plot(pred)
#' stopCluster(cl)
#' }
#' @export
predict.trainOcc <- function(object, newdata, type = "prob", allowParallel=TRUE, returnRaster=TRUE, mask=NULL, ...) { 
  
  if (is.data.frame(newdata) | is.matrix(newdata)) 
  {
    predictions <- predict.train(object=object, newdata=newdata, type=type, ...)[, 'pos']
  } 
  else if (.is.raster(newdata)) 
  {
    if ( any(search()%in%"package:foreach") ) #require('spatial.tools', quietly=TRUE)
    {
      if (is.null(mask)) 
      {
        predictions <- 
          spatial.tools::predict_rasterEngine(object, 
                                              newdata=newdata,
                                              type = type, ...)
      } else
      {
        predictions <- 
          spatial.tools::rasterEngine(inraster=newdata, 
                                      mask=mask,
                                      fun=.trainOcc_raster_predict,
                                      processing_unit="chunk",
                                      args=list(ocModel=object,type="prob",
                                                disable_masking=FALSE))
      }
    } else 
    {
      predictions <- predict(object=newdata, model=object, type=type, fun=predict.train)
    }
    if (nlayers(predictions)==1 & class(predictions)[[1]]!="RasterLayer")
      predictions <- raster(predictions, layer=1)
  } else if (class(newdata)=='rasterTiled') 
  {
    predictions <- predict(object=newdata, model=object, type = type, allowParallel=allowParallel, ...)
  }
  
  
  if ( (class(newdata)=='rasterTiled' | .is.raster(newdata) ) & !returnRaster  ) {
    if (!is.null(mask)) {
      predictions <- predictions[which(!is.na(values(mask)))]
    } else {
      predictions <- predictions[]
    }
  }
  
  return(predictions)
}