#'@export
write_hist <- 
  function (model, x_un, modRows=NULL, thDepPerf=FALSE,
            folder_out=NULL, format="png") {
    
    if (is.null(folder_out)) {
      folder_out <- paste(tempdir(), "\\iterOneClass", sep="")
      cat("\nSave results in ", folder_out, "\n\n")
      dir.create(folder_out, showWarnings=FALSE)
    }
    dir.create(folder_out, showWarnings = FALSE, recursive = TRUE)
    
    if (is.null(modRows))
      modRows <- 1:nrow(model$results)
    
    for (m in modRows) {
      cat(m, "\n")
      model <- update(model, modRow=m)
      if (thDepPerf) {
        hop <- holdOutPredictions(model, aggregate=TRUE)
        data <- dataForSummaryFunction(hop)
        thresholds <- seq(min(data$pos), 
                          max(data$pos), 
                          length.out=100)[2:99]
        perf <- puSummaryThLoop(data, 
                                thresholds = thresholds,
                                returnAll=TRUE)
      }
      
      pred <- predict(model, x_un)
      
      graphics.off()
      if (format == "png") {
        png(paste(folder_out, "/", sprintf("%03d", m), 
                  "_histogram", ".png", sep=""))
      } else if (format == "pdf") {
        pdf(paste(folder_out, "/", sprintf("%03d", m), 
                  "_histogram", ".pdf", sep=""))
      } else {
        error ('Supported format: \'pdf\', \'png\'.')
      }
      
      h <- hist(model, pred)
      y <- c(0, h$ylim[2])
      
      if (thDepPerf) {
        scaleMetric <- function ( x, y )
          approx(range(x), y, xou=x)$y
        #lines(perf$th, perf$tpr*h$ylim[2], lwd=2, col="darkgrey")
        #lines(perf$th, perf$ppp*h$ylim[2], lwd=2, col="darkgrey")
        #lines(perf$th, perf$puP*h$ylim[2], lwd=2, col="darkgrey")
        lines(perf$th, scaleMetric(perf$puF, y), lwd=2)
        lines(perf$th, scaleMetric(perf$puF1, y), lwd=2, lty=2)
        dev.off()
      }
      if (ncol(model$trainingData)==3) {
        graphics.off()
        if (format == "png") {
        png(paste(folder_out, "/", sprintf("%03d", m), "_featurespace", 
                  ".png", sep=""))
      } else if (format == "pdf") {
        pdf(paste(folder_out, "/", sprintf("%03d", m), "_featurespace", 
                  ".pdf", sep=""))
      } else {
        error ('Supported format: \'pdf\', \'png\'.')
      }
        featurespace(model)
        dev.off()
      }
      
      save(pred, perf, 
           file = paste(folder_out, "/hist_model-", sprintf("%03d", m), 
                        ".RData", sep="") )
      gc()
      
    }
    
  }