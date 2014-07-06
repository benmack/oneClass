require(caret)
require(raster)
require(pROC)
require(trainOcc)
require(RColorBrewer)


clrs <- list(greys = brewer.pal(9, "Greys"), 
             pos = trainOcc:::.clrs("PN")[[1]],
             neg = "#d6604d",
             un = trainOcc:::.clrs("PU")[[2]],
             pn <- c(trainOcc:::.clrs("PN")[[1]], "#d6604d"), 
             pu <- trainOcc:::.clrs("PU"))

seed <- 1#234567890
tr.n <- list(pos = 20, 
             neg = 20, 
             un = 500)
te.n <- 1000
########################################
### DATA
data(bananas)
y <- bananas$y
x <- bananas$x

### test data
set.seed(seed)
te.i <- sample(ncell(x), te.n)
te.coord <- xyFromCell(y, te.i)
te.y <- y[te.i]
te.x <- x[te.i]


### training data
set.seed(seed)
tr.pos.i <- sample(which(y[]==1), tr.n$pos)
set.seed(seed)
tr.neg.i <- sample(which(y[]!=1), tr.n$neg)
set.seed(seed)
tr.un.i <- sample(ncell(x), tr.n$un)

tr.pn.i <- c(tr.pos.i, tr.neg.i)
tr.pu.i <- c(tr.pos.i, tr.un.i)

tr.pn.coord <- xyFromCell(y, tr.pn.i)
tr.pu.coord <- xyFromCell(y, tr.un.i)

tr.pn.y <- y[tr.pn.i]
tr.pu.y <- rep(c(1,0), c(tr.n$pos, tr.n$un))

tr.pn.x <- x[tr.pn.i]
tr.pu.x <- x[tr.pu.i]

plot(tr.pu.x,  pch=ifelse(tr.pu.y==1, 21, 4), 
     bg=ifelse(tr.pu.y==1, clrs$pos, clrs$un), 
     cex=ifelse(tr.pu.y==1, 2, 1), lwd=2)
plot(tr.pn.x,  pch=ifelse(tr.pu.y==1, 21, 4), lwd=2)

########################################
### BINARY CLASSIFICATION
tuneGrid <- expand.grid(sigma=c(.01, .05, .1, .5, 1, 5),  
                        C=2^seq(-10, 15, 3))
bc <- train(x=tr.pn.x, y=as.factor(tr.pn.y), method="svmRadial", 
            tuneGrid=tuneGrid, prob.model = TRUE)

clrs <- featurespace(x=bc$finalModel, thresholds=0.5, x.tr=cbind(tr.pn.x, y=tr.pn.y),
             col=2, expandColors=FALSE, type="probabilities")

pred.bc <- predict(bc$finalModel, newdata=x[], type="probabilities")
pred.bc <- setValues(y, pred.bc[,2])
plot(pred.bc, col=clrs$colors, breaks=clrs$levels)

