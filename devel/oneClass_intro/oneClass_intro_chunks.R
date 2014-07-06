# setwd("inst/docs/")

################################################################################
### Installation
### ---------------------------------------------------------------------------- 
# <<inst, eval=FALSE>>=
# require(devtools)
# install_github('benmack/trainOcc')
# ### ---------------------------------------------------------------------------- # 

################################################################################
### Synthetic data:bananas

### ---------------------------------------------------------------------------- 
# <<loadpkg, echo=-6, message=FALSE, warning=FALSE, fig.width=16, fig.height=4, fig.cap='The class membership (left), and the two features (middle and right) of the synthetic bananas data set.'>>=
require(trainOcc)
require(caret)
require(raster)
data(bananas)
plot(stack(bananas$y, bananas$x), nc=3)
require(gridExtra) # also loads grid
#### ----------------------------------------------------------------------------
# <<traintestdata, tidy=FALSE>>=
seed <- 123456
tr.x <- bananas$tr[, -1]
tr.y <- puFactor(bananas$tr[, 1], positive=1)
set.seed(seed)
te.i <- sample(ncell(bananas$y), 1000)
te.x <- extract(bananas$x, te.i)
te.y <- extract(bananas$y, te.i)
### ----------------------------------------------------------------------------

### ---------------------------------------------------------------------------- 
# <<featurespace_trainTest, echo=-1, fig.width=8, fig.height=4, out.width='0.8\\textwidth'>>=
par(mfrow=c(1,2), mgp=c(1.75, .75, 0), mar=c(3, 3, .1, .1))
plot(tr.x, pch=ifelse(tr.y=="pos", 16, 4) )
plot(te.x, pch=ifelse(te.y==1, 16, 4) )
### ---------------------------------------------------------------------------- # 

################################################################################
### Model selection in the absence of PN-data.

### ---------------------------------------------------------------------------- 
# <<default_trainOcc, eval=FALSE, echo=-c(1, 12)>>=
set.seed(seed)
index <- createFolds( tr.y, k=10, returnTrain=TRUE )
trControl <- trainControl(method='cv', 
                          index=index, 
                          summaryFunction = puSummary,  # PU-performance metrics
                          classProbs=TRUE,              # important 
                          savePredictions = TRUE,       # important
                          returnResamp = 'all')         # for resamples.train 
oc <- trainOcc( x = tr.x, y = tr.y, trControl = trControl ) 
pred <- predict(oc, bananas$x)
pred <- readAll(pred)
### ---------------------------------------------------------------------------- # 

### ---------------------------------------------------------------------------- # default_trainOcc_load, echo=FALSE, warning=FALSE>>=
# <<default_trainOcc_load, echo=FALSE, warning=FALSE>>=
fname <- 'trainOcc_intro_rdata/oc_default.RData'
obj <- c("oc", "pred")
if (!file.exists(fname)) {
  save(list=obj, file=fname)
} else {
  load(fname)
}
### ----------------------------------------------------------------------------

### ---------------------------------------------------------------------------- 
# hist_featurespace, eval=FALSE, message=FALSE>>=
plot(pred)
hist(oc, pred)
featurespace(oc, th=0)
### ----------------------------------------------------------------------------  
#\begin{multicols}{3}
### ---------------------------------------------------------------------------- 
# <<img_plot, echo=FALSE, out.width='.95\\linewidth'>>=
plot(pred)
### ----------------------------------------------------------------------------  
### ---------------------------------------------------------------------------- 
# <<hist_plot, echo=FALSE, out.width='.95\\linewidth', warning=FALSE>>=
hist(oc, pred)
### ----------------------------------------------------------------------------  
### ---------------------------------------------------------------------------- 
# <<featurespace_plot, echo=FALSE, out.width='.95\\linewidth', message=FALSE>>=
featurespace(oc, th=0)
### ---------------------------------------------------------------------------- # 
#\end{multicols}

### ---------------------------------------------------------------------------- 
# <<levelplots, out.width='.5\\textwidth'>>=
trellis.par.set(caretTheme()) # nice colors from caret
plot(oc, metric = 'puF', plotType = "level")
plot(oc, metric = 'puAuc', plotType = "level")
### ---------------------------------------------------------------------------- # 

### ---------------------------------------------------------------------------- 
# <<print_f, size='footnotesize'>>=
# oc # --> prints the whole large table with performance metrics of all models
sort(oc, printTable=TRUE, rows=1:10, by='puF', digits=2)
### ---------------------------------------------------------------------------- # 

### ---------------------------------------------------------------------------- 
# <<print_auc, size='footnotesize'>>=
sort(oc, printTable=TRUE, rows=1:10, by='puAuc', digits=2)
### ---------------------------------------------------------------------------- # 

### ---------------------------------------------------------------------------- 
# <<candidates, tidy=FALSE>>=
puF.ranking   <- sort (oc, by='puF', print=FALSE)
puAuc.ranking <- sort (oc, by='puAuc', print=FALSE)

candidates <- as.numeric(unique(c(rownames(puF.ranking),
                                  rownames(puAuc.ranking)) [ 
                                    rep(1:10, each=2)+rep(c(0, 10), 10) ]))
candidates[1:10]
### ---------------------------------------------------------------------------- # 

### ---------------------------------------------------------------------------- 
# <<resamps_echo, eval=FALSE>>=
resamps <- resamples(oc, modRow=candidates[1:10])
bwplot(resamps, metric='puF')
### ---------------------------------------------------------------------------- # 

### ---------------------------------------------------------------------------- 
# <<resamps_eval, echo=FALSE, out.width='0.75\\textwidth'>>=
fname <- 'trainOcc_intro_rdata/resamps.RData'
obj <- c("resamps")
if (!file.exists(fname)) {
  save(list=obj, file=fname)
} else {
  load(fname)
}
bwplot(resamps, metric='puF')
### ---------------------------------------------------------------------------- # 

### ---------------------------------------------------------------------------- # updateCandidates, eval=FALSE>>=
oc.candidate1 <- update(oc, u=bananas$x, modRow=candidates[1])
oc.candidate2 <- update(oc, u=bananas$x, modRow=candidates[2])
oc.candidate3 <- update(oc, u=bananas$x, modRow=candidates[3])
### ---------------------------------------------------------------------------- # 

### ---------------------------------------------------------------------------- # update_candidates_load, echo=FALSE>>=
# save(oc.candidate1, oc.candidate2, oc.candidate3, file='trainOcc_intro_rdata/ocUpdated.RData')
fname <- 'trainOcc_intro_rdata/ocUpdated.RData'
obj <- c("oc.candidate1", "oc.candidate2", "oc.candidate3")
if (!file.exists(fname)) {
  save(list=obj, file=fname)
} else {
  load(fname)
}
load(fname)
### ---------------------------------------------------------------------------- # 
#\begin{multicols}{3}
### ---------------------------------------------------------------------------- 
# update1_hist, echo=FALSE, warning=FALSE, message=FALSE>>=
hist(oc.candidate1, ylim=c(0, .2), main=candidates[1], cex.main=3, cex.axis=1, cex.lab=1.5)
### ---------------------------------------------------------------------------- # 
### ---------------------------------------------------------------------------- 
# update1_fs, echo=FALSE, warning=FALSE, message=FALSE>>=
featurespace(oc.candidate1, th=0)
### ---------------------------------------------------------------------------- # 
### ---------------------------------------------------------------------------- 
# update2_hist, echo=FALSE, warning=FALSE, message=FALSE>>=
hist(oc.candidate2, ylim=c(0, .2), main=candidates[2], cex.main=3, cex.axis=1, cex.lab=1.5)
### ---------------------------------------------------------------------------- # 
### ---------------------------------------------------------------------------- 
# update2_fs, echo=FALSE, warning=FALSE, message=FALSE>>=
featurespace(oc.candidate2, th=0)
### ---------------------------------------------------------------------------- # 
### ---------------------------------------------------------------------------- 
# update3_hist, echo=FALSE, warning=FALSE, message=FALSE>>=
hist(oc.candidate3, ylim=c(0, .2), main=candidates[3], cex.main=3, cex.axis=1, cex.lab=1.5)
### ---------------------------------------------------------------------------- # 
### ---------------------------------------------------------------------------- 
# update3_fs, echo=FALSE, warning=FALSE, message=FALSE>>=
featurespace(oc.candidate3, th=0)
### ---------------------------------------------------------------------------- # 
#\end{multicols}

################################################################################
### Evaluation with PN-data

### ---------------------------------------------------------------------------- 
# <<accuracyTh0, size='footnotesize'>>=
te.pred <- predict(oc, bananas$x[te.i])
ev <- dismo::evaluate(p=te.pred[te.y==1], a=te.pred[te.y!=1])
ev.th0 <- evaluateAtTh(ev, th=0)
ev.th0
### ---------------------------------------------------------------------------- # 

If we are interested in the evolution of the accuracy over different thresholds 
we can also specify a vector of threshold or just leave the argument away in which 
case several thresholds are generated automatically over the range of the predictive values.
From this we might also derive maximum achievable accuracy, i.e. the accuracy at the threshold which optimizes a given accuracy metric, e.g. the $\kappa$ coefficient.

### ---------------------------------------------------------------------------- 
# <<accuracyThs, eval=FALSE>>=
plot(ev)
evaluateAtTh(ev, th='max.kappa')
### ---------------------------------------------------------------------------- # 

#\begin{multicols}{2}
### ---------------------------------------------------------------------------- 
# <<evalDepTh_plot, warning=FALSE, message=FALSE, echo=FALSE>>=
#par(cex=2)
plot(ev)
### ---------------------------------------------------------------------------- # 
### ---------------------------------------------------------------------------- 
# <<eval_maxK, warning=FALSE, message=FALSE, echo=FALSE, size='footnotesize'>>=
print(evaluateAtTh(ev, th='max.kappa'))
### ---------------------------------------------------------------------------- # 
#\end{multicols}

### ---------------------------------------------------------------------------- 
# <<ev_modsel, eval=FALSE>>=
ev.modsel <- evaluateOcc(oc, y=te.y, u=te.x, allModels=TRUE, positive=1)
evList <- print(ev.modsel, invisible=TRUE)
### ---------------------------------------------------------------------------- # 

### ---------------------------------------------------------------------------- 
# <<ev_modsel_load, echo=FALSE>>=
fname <- 'trainOcc_intro_rdata/ev_modsel.RData'
obj <- c("ev.modsel", "evList")
if (!file.exists(fname)) {
  save(list=obj, file=fname)
} else {
  load(fname)
}
### ---------------------------------------------------------------------------- # 

### ---------------------------------------------------------------------------- 
# <<evaluation_evList, echo=FALSE>>=
evList <- print(ev.modsel, invisible=TRUE)
### ---------------------------------------------------------------------------- # 

#\begin{multicols}{3}
### ---------------------------------------------------------------------------- 
# <<evalModsel_puF, warning=FALSE, message=FALSE, echo=-1>>=
par(cex=2, mgp=c(1.5, .5, 0))
plot(evList$puF, evList$mxK.K, xlab="puF", ylab="max. kappa")
### ---------------------------------------------------------------------------- # 
### ---------------------------------------------------------------------------- 
# <<evalModsel_puAuc, warning=FALSE, message=FALSE, echo=-1>>=
par(cex=1, mgp=c(1.5, .5, 0))
plot(evList$puAuc, evList$mxK.K, xlab="puAuc", ylab="max. kappa")
### ---------------------------------------------------------------------------- # 
### ---------------------------------------------------------------------------- 
# <<evalModsel_tpr, warning=FALSE, message=FALSE, echo=-1>>=
par(cex=1, mgp=c(1.5, .5, 0))
plot(evList$ppp, evList$tpr, xlab="probability of positive prediction", ylab="true positive rate")
### ---------------------------------------------------------------------------- # 
#\end{multicols}

################################################################################
### Parallel processing

### ---------------------------------------------------------------------------- 
# <<eval=FALSE>>=
require(foreach)
require(doParallel)
cl <- makeCluster(detectCores()-1) # leave one core free
registerDoParallel(cl)
### ---------------------------------------------------------------------------- # 

################################################################################
### Prediction of large raster data

### ---------------------------------------------------------------------------- 
# <<<rasterTiled, size='footnotesize', message=FALSE>>=
require(rasterTiled)
rt <- rasterTiled(bananas$x)
names(rt)

rt # also estimates the approximate size of the tiles in memory
### ---------------------------------------------------------------------------- # 

setwd("D:/github/trainOcc/")
