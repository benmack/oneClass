#-----------------------------------------------------------
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

#-----------------------------------------------------------
trs <- getTrainset(100, 200, 123)
trs.p <- trs[trs$y==1, ]
plot(trs[, -1], col=trs$y+1)
require(kernlab)
s.p <- t(sapply(1:500, function(i) {
  set.seed(i)
  sigest(as.matrix(trs.p[, -1]))}))
s <- t(sapply(1:500, function(i) {
  set.seed(i)
  sigest(as.matrix(trs[, -1]))}))
head(s)


par(mfrow=c(1, 3))
sapply(1:3, function(i) boxplot(data.frame(s=s[, i], s.p=s.p[, i])))

tg <- expand.grid(sigma=c(min(s, s.p), 3 ),  #max(s, s.p)),
                  cNeg=1,
                  cMultiplier=c(1, 100))
mod <- trainOcc(x=trs[, -1], y=trs$y, tuneGrid=tg, verbose=F)
mod$bestTune

mod.min = update(mod, modRow=1)
featurespace(mod.min, main="MIN")
mod.min = update(mod, modRow=2)
featurespace(mod.min, main="MIN")

mod.max = update(mod, modRow=3)
featurespace(mod.max, main="MAX", th=0)
mod.max = update(mod, modRow=4)
featurespace(mod.max, main="MAX", th=0)

