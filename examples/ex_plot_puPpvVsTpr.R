### Example for plot_PPPvsTPR

data(bananas)
### get some models: 
  # one-class svm
ocsvm <- trainOcc (x = tr[, -1], y = tr[, 1], method="ocsvm", 
                tuneGrid=expand.grid(sigma=c(seq(0.01, .09, .02), seq(0.1, .9, .1)), 
                                     nu=seq(.05,.55,.1)) )
# biased svm
biasedsvm <- trainOcc (x = tr[, -1], y = tr[, 1], method="bsvm", 
                       tuneGrid=expand.grid(sigma=c(0.1, 1), 
                                            cNeg=2^seq(-4, 2, 2), 
                                            cMultiplier=2^seq(4, 8, 2) ) )
# compare with PPPvsTPR-plot
plot_PPPvsTPR(ocsvm, xlim=c(0,1), ylim=c(0,1), metric="negD01")
plot_PPPvsTPR(biasedsvm, xlim=c(0,1), ylim=c(0,1), metric="negD01", 
                add=TRUE, col="red")
