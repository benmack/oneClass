################################################################################
#' @aliases predict
#' @aliases predict.oneClass
#' 
#' @title Predict method for \code{\link{oneClass}} object 
#'
#' @description ... .
#' 
#' @note \code{type='probs'} does NOT return probabilities but the continuous 
#' decision values of the classifier, e.g. distances in the case of the biased svm) and not probabilities!
#' 
#' @param object a \code{\link{oneClass}} object 
#' @param newdata new data to predict on. 
#' @param type default is 'prob' which however returns the continuous decision values (see note!)
#' @param allowParallel ...
#' @param returnRaster default \code{TRUE}
#' @param mask if given and if \code{returnRaster=TRUE} only predictions of the valid cells are returned.
#' @param ... other arguments that can be passed to \code{\link{predict.train}}.
#' @return committee predictions and if desired predictions of individual classifiers.
#' @examples
#' # to do
#' @method predict oneClass
#' @export
predict.oneClass <- function(object, newdata, type = "prob", allowParallel=TRUE, returnRaster=TRUE, mask=NULL, ...) { ##ä# , mask=NULL ???
  
  if (is.data.frame(newdata) | is.matrix(newdata)) {
    
    
    predictions <- predict.train(object=object, newdata=newdata, type=type, ...)[, 'pos']
    
    
  } else if (.is.raster(newdata)) {
    
    if ( any(search()%in%"package:foreach") & require('spatial.tools', quietly=TRUE) & 
           allowParallel & is.null(mask)) { 
      predictions <- predict_rasterEngine(object, newdata=newdata, type = type, ...)
    } else {
      predictions <- predict(object=newdata, model=object, type=type, fun=predict.train)
    }
    
    
  } else if (class(newdata)=='rasterTiled') {
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