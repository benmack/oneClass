require(doParallel)
try(stopCluster(cl))
cl <- makeCluster(detectCores())
registerDoParallel(cl)
require(ROCR)
require(dismo)

data(bananas)
y <- bananas$y[]
table(y)/sum(c(144000,16000))

idx.teP <- sample(which(y==1), 100)
idx.teN <- sample(which(y==-1), 1000)
teP <- bananas$x[idx.teP]
teN <- bananas$x[idx.teN]

tuneGrid <- expand.grid(sigma=c(1:10)*rep(c(.1, 1), each=10), 
                        nu=c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25))
model <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], 
                  method="ocsvm", tuneGrid=tuneGrid)
featurespace(model)

# loop through thresholds and get negD01 criteria
nThs <- 100
nModels <- nrow(model$results)
ans <- numeric(nThs*nModels)
df <- data.frame(model=rep(1:nModels, each=nThs), th=ans,
                 ths=ans, TPR=ans, negD01=ans, puF=ans, kappa=ans)

row <- 0
cat("ToDo: ", nModels, "\n")
for (iM in 1:nModels) {
  cat(iM, ". ")
  hop <- holdOutPredictions(model, modRow=iM, aggregate=T)
  nPos <- length(hop$pos)
  nUn <- length(hop$un)
  rng <- quantile(c(hop$pos, hop$un), c(0.02, 0.98))
  ths <- seq(rng[1], rng[2], length.out=nThs)
  df$th[df$model==iM] <- ths
  # With test data:
  model <- update(model, modRow=iM)
  predP <- predict(model, teP)
  predN <- predict(model, teN)
  ev <- evaluate(p=predP, a=predN, tr=ths)

  for (iT in 1:nThs) {
    row <- row + 1
    df$TPR[row] <- sum(hop$pos > ths[iT])/nPos
    df$PPP[row] <- sum(hop$un > ths[iT])/nUn
    df$negD01[row] <- .negD01(df$TPR[row], df$PPP[row])
    df$puF[row] <- df$TPR[row]^2 / df$PPP[row]
    df$acc[row] <- ev@kappa[iT]
  }
}

fit.negD01 <- interp(df$PPP, df$TPR, df$negD01, 
                     xo=seq(0, 1, length = 100),
                     yo=seq(0, 1, length = 100),
                     duplicate="strip")
fit.puF <- interp(df$PPP, df$TPR, df$puF, 
                     xo=seq(0, 1, length = 100),
                     yo=seq(0, 1, length = 100),
                     duplicate="strip")
fit.acc <- interp(df$PPP, df$TPR, df$acc, 
                   xo=seq(0, 1, length = 100),
                   yo=seq(0, 1, length = 100),
                   duplicate="strip")

par(pty="s")
image(fit.negD01, col=brewer.pal(11, "RdBu"))
points(rep(0, 10), rep(1, 10), cex=c(1, seq(10, 100, length.out=10)))
points(df$PPP, df$TPR, pch=16, cex=0.1)

par(pty="s")
image(fit.puF, col=brewer.pal(11, "RdBu"))
points(rep(0, 10), rep(1, 10), cex=c(1, seq(10, 100, length.out=10)))
points(df$PPP, df$TPR, pch=16, cex=0.1)

par(pty="s")
image(fit.acc, col=brewer.pal(11, "RdBu"))
points(rep(0, 10), rep(1, 10), cex=c(1, seq(10, 100, length.out=10)))
points(df$PPP, df$TPR, pch=16, cex=0.1)
