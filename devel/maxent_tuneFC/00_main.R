# Phillips, S. J., & Dudík, M. (2008). Modeling of species distributions with Maxent: New extensions and a comprehensive evaluation. Ecography, 31(2), 161–175. doi:10.1111/j.0906-7590.2008.5203.x
# Shcheglovitova, M., & Anderson, R. P. (2013). Estimating optimal complexity for ecological niche models: A jackknife approach for species with small sample sizes. Ecological Modelling, 269, 9–17. doi:10.1016/j.ecolmodel.2013.08.011
# source("devel/maxent_tuneFC/new_maxent_model.R")
require(dismo)
require(oneClass)
data(bananas)
y <- bananas$y[]
x <- bananas$x[]
getTrainset <- function(nP, nU, seed) {
  set.seed(seed)
  xTr <- as.data.frame(rbind(x[sample(which(y==1), nP), ],
                             x[sample(nrow(x), nU), ]))
  y <- rep(c(1, 0), c(nP, nU))
  return(data.frame(y=y, x1=xTr[, 1], x2=xTr[, 2]))
}
getTestset <- function(nP, nN, seed) {
  set.seed(seed)
  xTr <- as.data.frame(rbind(x[sample(which(y==1), nP), ],
                             x[sample(which(y!=1), nU), ]))
  y <- rep(c(1, -1), c(nP, nN))
  return(data.frame(y=y, x1=xTr[, 1], x2=xTr[, 2]))
}
trainMaxent <- function(nP, nU, seed, withDismo=TRUE) {
  trset <- getTrainset(nP, nU, seed)
  if (withDismo) {
    mod <- dismo::maxent(x=trset[, -1], trset[, 1])
  } else {
    grid <- expand.grid(fc="D", beta = 1)
    mod <- trainOcc(trset[, -1], trset[, 1],
                   method="maxent", tuneGrid=grid,
                   verbose=FALSE)
  }
  return(mod)
}

### Train Maxent models with different |P| and open html
for (nP in c(9, 10, 14, 15, 79, 80)) {
  mod <- trainMaxent(nP, 100, 123)
  print(mod)
}

nPte=100
nNte=100
teSet <- getTestset(nPte, nNte, 1234)

nP=30
nU=100
trset <- getTrainset(nP, nU, 123)

# Many combinations of feature classes are possible; 
# common combinations used are (Phillips & Dudik 2008)
fc <- c("L", "LQ", "H", "HQ", "T", "HQPT")
fc <- c("LQ", "HQ", "HQPT")

grid <- expand.grid(fc=fc,
                    beta = c(.1, 1, 10))
me <- trainOcc(trset[, -1], trset[, 1], 
               trControl=trainControl(method="cv", number=10,
                                      summaryFunction=puSummary, verboseIter=T),
               method="maxent", tuneGrid=grid, 
               path="devel/maxent_tuneFC/maxentResults")

ev <- evaluateOcc(me, te.u=teSet[, -1], te.y=teSet$y, positive=1,
                  allModels=TRUE)


featurespace(me)

# ---- 
# TEST IF DEFAULT RUNS ARE EQUAL

### Train Maxent models with different |P| and open html
for (nP in c(9, 10, 14, 15, 79, 80)) {
  modDismo <- trainMaxent(nP, 100, 123, withDismo=TRUE)
  print(modDismo)
  mod <- trainMaxent(nP, 100, 123, withDismo=FALSE)
  print(mod$finalModel)
  
  # rm elements that should differ
  mod$finalModel@path <- ""
  mod$finalModel@html <- ""
  modDismo@path <- ""
  modDismo@html <- ""
  
  cat(sprintf("*********\nNumber of samples: %d", nP))
  cat(sprintf("\nDismo and default trainOcc models are %sidentical.\n", 
              ifelse(identical(mod$finalModel, modDismo), "", "NOT ")))
  # n <- readline("Press RETURN to continue.")
}

### WITH A COSTUM MODEL
# source("devel/maxent_tuneFC/newMaxent_modelInfo.R")
# me <- trainOcc(trset[, -1], trset[, 1], 
#                trControl=trainControl(method="cv", number=10,
#                summaryFunction=puSummary, verboseIter=T),
#                method=modelInfo, tuneGrid=grid, nPos=100, 
#                path="devel/maxent_tuneFC/maxentResults01")

