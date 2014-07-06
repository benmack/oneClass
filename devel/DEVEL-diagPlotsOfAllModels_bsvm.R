tr <- bananas$tr

method <- "biasedsvm"

oc <- trainOcc (x = tr[, -1], y = tr[, 1])

require(doParallel)
registerDoParallel(4)

for (i in 1:nrow(oc$results)) {
  oc <- update(oc, modRow=i)
  pred <- predict( oc, bananas$x )
  pdf(paste("develScripts/diagPlotsOfAllModels_", method, "/plotOfModel-", i, ".pdf", sep=""))
  hist(oc, predUn=pred, th=0, main=paste(names(oc$bestTune), oc$bestTune, collapse=" / "))
  featurespace(oc, th=0, main=paste(names(oc$bestTune), oc$bestTune, collapse=" / "))
  plot_PPPvsTPR(oc)
  points(oc$results$ppp[i], oc$results$tpr[i], pch=8, col="red", 
         main=paste(names(oc$bestTune), oc$bestTune, collapse=" / "))
  dev.off()
}