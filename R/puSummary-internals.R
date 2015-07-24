.negD01 <- function (tpr, ppp)
   -sqrt( (1-tpr)^2 + ppp^2 )

.nSv.trainOcc <- function(x) {
  nSV <- numeric(nrow(x$results))
  for (m in 1:nrow(x$results)) {
    nSV[m] <- nSV(update(x, modRow=m)$finalModel)
  }
  return(nSV)
}

