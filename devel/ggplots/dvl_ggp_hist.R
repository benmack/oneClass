data(bananas)
### an underfitted model 
oc <- trainOcc (x = bananas$tr[, -1], y = bananas$tr[, 1], 
                tuneGrid=expand.grid(sigma=0.1, 
                                     cNeg=0.5, 
                                     cMultiplier=16))
### predict 10% or the unlabeled data and plot 
# the diagnostic distributions plot
# and the model in the 2D feature space 
set.seed(123)
idx.pred <- sample(400*400, 16000)
hist(oc, predict(oc, bananas$x[][idx.pred,]), th=0, add_calBoxplot=F)

# ---- 
# arguments
x=oc
predUn=bananas$x[][idx.pred,]
th=0
cab=NULL
main=NULL
ylim=NULL
breaks='Scott'
col="grey"
border=NA
xlim=NULL
add_calBoxplot=TRUE
noWarnRasHist=FALSE

# ---- 
# function body 

if (!is.null(x$holdOut$pos) & !is.null(x$holdOut$un)) {
  hop <- list(pos = x$holdOut$pos, un = x$holdOut$un)
} else {
  hop <- holdOutPredictions(x, aggregate=TRUE)
}

trSet_pos <- x$trainingData[x$trainingData[,ncol(x$trainingData)]=="pos", 
                            -ncol(x$trainingData)]
pred_tr_pos <- predict(x, trSet_pos)


if (!is.null(predUn)) {
  predictive.value <- predUn
  if (is.null(xlim)) {
    if (.is.raster(predictive.value)) 
    {
      if (!is.finite(predictive.value@data@min) |
          !is.finite(predictive.value@data@max))
        predictive.value <- setMinMax(predictive.value)
      xlim <- c(predictive.value@data@min, 
                predictive.value@data@max)
    } else {
      xlim <- range(predictive.value, na.rm=T)
    }
  }
} else if (!is.null(x$predUn)) {
  predictive.value <- x$predUn
  if (is.null(xlim))
    xlim <- range(predictive.value, na.rm=T)
} else if ( is.null(predUn) & is.null(x$predUn) ) {
  warning('No predicted unlabeled data found.\nUnlabeled hold-out predictions used to build the histogram.')
  predictive.value <- hop$un
  if (is.null(xlim))
    xlim <- range(c(unlist(hop$un), unlist(hop$pos)), na.rm=T)
}

if (.is.raster(predictive.value) & noWarnRasHist) {
  oldw <- getOption("warn")
  options(warn = -1)
  h <- hist(predictive.value, plot=FALSE, breaks=breaks) # , ... TODO
  options(warn = oldw)
} else {
  h <- hist(predictive.value, plot=FALSE, breaks=breaks)  # , ... TODO
}
h$xname <- "predictive value"

require(ggplot2)


h <- ggplot(data = data.frame(predictive.value), aes(x = x1)) +
  geom_histogram(bins=30) +
  scale_x_continuous(expand = c(0,0), limit = xlim)

###############################################################################
### set defaults if necessary 
### ylim
# browser()
if (is.null(ylim)) {
  ans <- boxplot(unlist(hop$pos), plot=FALSE)$stat[1]
  maxInRelevantRange <- max(h$density[h$mids>=ans & is.finite(h$density)])
  # ylim <- .ylimForHist( h, positives=unlist(hop$pos) )
  if (maxInRelevantRange < (max(h$density[is.finite(h$density)]))) {
    ylim <- c(0, maxInRelevantRange*2) # range(h$density)
  } else {
    ylim <- c(0, maxInRelevantRange) # range(h$density)
  }
  if (any(!is.finite(ylim)))
    ylim <- c(0, max( h$density ))
}

if (is.null(main)) {
  parvals <- sapply(x$bestTune, 
                    function(parval) 
                      ifelse(is.numeric(parval), signif(parval,3), as.character(parval)))
  signif(x$bestTune[is.numeric(x$bestTune)],3)
  main <- paste(paste(names(x$bestTune), parvals, collapse=" / "), 
                paste("\nrow", modelPosition(x)$row, "/ #U", length(predictive.value)))
}
clrs <- .clrs('PU')

###############################################################################
### ### TPR and PPP
# browser()
#   prb <- seq(0, 1, .01)
#   prb.scld <- approx(c(0,1), ylim, prb)$y
#   percentiles.pnp <- quantile(predUn, prb)
#   percentiles.pnp.tr.un <- quantile(hop$un, prb)
#   percentiles.tpr <- quantile(hop$pos, prb)

ylim[1] <- 0-diff(c(0,ylim[2]))*.15
if (!is.null(cab) & is.list(cab)) {
  col <- rep(NA, length(h$mids))
  for(i in 1:(length(cab$colors))) {
    if (i==1) {
      idx <- h$mids<cab$breaks[i]
    } else if (i==length(cab$colors)) {
      idx <- h$mids>=cab$breaks[i-1]
    } else {
      idx <- h$mids>=cab$breaks[i-1] & h$mids<cab$breaks[i]
    }
    col[idx] <- cab$colors[i]
  }
  
}

# plot(h, freq=FALSE, ylim=ylim, xlim=xlim, main=main, col=col, border=border, ...)
#   legend("topright", c("TPR", "1-PPP (train, U)", "1-PPP (all U)"), 
#          lwd=c(2,2,2), lty=c(1,1,5), col=c("black", "black", clrs$pos))
#   
#   axis(4, at=approx(c(0,1), c(0, ylim[2]), c(0, .25, .5, .75, 1))$y, labels=c(0, .25, .5, .75, 1) )
#   lines(percentiles.tpr, ylim[2]-prb.scld, lwd=2, col=clrs$pos)
#   lines(percentiles.pnp.tr.un, ylim[2]-prb.scld, lwd=2)
#   lines(percentiles.pnp, ylim[2]-prb.scld, lwd=2, lty=5)
#   
# bxwx <- abs(ylim[1])*.75
# if (add_calBoxplot)
#   boxplot(pred_tr_pos, frame=FALSE, axes=FALSE, y=0, horizontal=TRUE, 
#           at=ylim[1]*.25, add=TRUE, boxwex=bxwx, col="#a6cee3")
# boxplot(unlist(hop$pos), frame=FALSE, axes=FALSE, y=0, horizontal=TRUE, 
#         at=ylim[1]*.5, add=TRUE, boxwex=bxwx, col=clrs$pos )
# 
# if ( !is.null(hop$un) ) {
#   boxplot(unlist("pred_val", length(hop$pos), 
#              ), y=0, horizontal=TRUE, axes=FALSE, 
#           at=ylim[1]*.9, add=TRUE, boxwex=bxwx, col=clrs$un) # , col=cols[1]
# }

df4bps <- data.frame(
  type = factor(c(rep("P(cal)", length(pred_tr_pos)), 
           rep("P(val)", length(hop$pos)), 
           rep("U", length(hop$un)))),
  values=c(pred_tr_pos, 
           hop$pos,
           hop$un))


bp <- ggplot(data = df4bps, aes(x = type, y = values, fill=type)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limit = xlim) +
  theme(legend.position="none")

require(gridExtra)

grid.arrange(h, bp, nrow=2, heights=c(0.8, 0.2))

if(!is.null(th)){
  for (i in 1:length(th))
    lines(rep(th[i], 2), y=c(0, ylim[2]), lwd=3)
}

invisible(list(h=h, ylim=ylim))
