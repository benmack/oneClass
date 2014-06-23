################################################################################
.constructOneClass <- function(x, u=u, mask=mask, time.train=time.train, 
                               funcCall=funcCall, funcCallUpdate=NULL, ...) {
  
  ### predict u
  dong <- proc.time()
  if (  class(u)=="RasterBrick" | 
          class(u)=="RasterStack" ) {
    pred.un.new <- predict(u, x, type='prob')
    #browser()
    if (!is.null(mask))
      pred.un.new <- extract(pred.un.new, which(!is.na(values(mask))))
    else
      pred.un.new <- pred.un.new[]
  } else {
    pred.un.new <- predict(x, u, type='prob')$pos
#    browser()
#    identical(pred.un.new, x$predUn)
  } 
  time.pred <- proc.time()-dong
  
  ### create the oneClass objct 
  newX <- structure( c ( x,
                       list(callOc = funcCall,
                            callUpdateOc = funcCallUpdate,
                            dotsOc = list(...), 
                            predUn = pred.un.new,
                            # raster = dummy$raster,
                            timeOc = list(train=time.train, pred=time.pred))),
                   class = c("oneClass", "train") )

# hist(pred.un)
# hist(newX$predUn)


  # names(trainbject)
  #  [1] "method"       "modelInfo"    "modelType"    "results"      "pred"         "bestTune"    
  #  [7] "call"         "dots"         "metric"       "control"      "finalModel"   "preProcess"  
  # [13] "trainingData" "resample"     "resampledCM"  "perfNames"    "maximize"     "yLimits"     
  # [19] "times" 
  return(newX)
}