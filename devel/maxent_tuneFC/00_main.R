# Phillips, S. J., & Dudík, M. (2008). Modeling of species distributions with Maxent: New extensions and a comprehensive evaluation. Ecography, 31(2), 161–175. doi:10.1111/j.0906-7590.2008.5203.x
# Shcheglovitova, M., & Anderson, R. P. (2013). Estimating optimal complexity for ecological niche models: A jackknife approach for species with small sample sizes. Ecological Modelling, 269, 9–17. doi:10.1016/j.ecolmodel.2013.08.011
source("devel/maxent_tuneFC/new_maxent_model.R")

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

trainMaxent <- function(nP, nU, seed) {
  trset <- getTrainset(nP, nU, seed)
  mod <- maxent(trset[, -1], trset[, 1])
return(mod)
}

for (nP in c(9, 10, 14, 15, 79, 80)) {
  mod <- trainMaxent(nP, 100, 123)
  print(mod)
}

nP=9
nU=100
trset <- getTrainset(9, 100, 123)
# trset <- cbind(trset, fact=factor(rep(c("A", "B"), c(nU, nP))))

# Many combinations of feature classes are possible; 
# common combinations used are (Phillips & Dudik 2008)
fc <- c("L", "LQ", "H", "HQ", "T", "HQPT")

grid <- expand.grid(fc=fc,
                   beta = c(.1, 1, 10))
# source("devel/maxent_tuneFC/newMaxent_modelInfo.R")
me <- trainOcc(trset[, -1], trset[, 1], 
               trControl=trainControl(method="cv", number=10,
               summaryFunction=puSummary, verboseIter=T),
               method=modelInfo, tuneGrid=grid, nPos=100, 
               path="devel/maxent_tuneFC/maxentResults01")

featurespace(me)

me <- update(me, modRow=1)

# betas 
betas <- c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 4, 8, 16)

expand.grid(fc=fc, beta=betas)

require(dismo)

