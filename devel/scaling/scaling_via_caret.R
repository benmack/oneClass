devtools::load_all(".")

#-----------------------------------------------------------
### 
data(bananas)
y <- bananas$y[]
x <- bananas$x[]
x[, 1] <- x[, 1] * 1000000
#-----------------------------------------------------------
### 
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
### 
trset = getTrainset(40, 100, 123)
plot(trset$x1, trset$x2, pch=trset$y)
mod <- trainOcc(trset[, -1], trset[, 1],
               verbose=FALSE)


