require(kernlab)
require(raster)
require(oneClass)

load_all('.')
data(bananas)
table(bananas$tr[, 1])
seed <- 2

set.seed(seed)
idx.p <- sample(which(bananas$y[]==1), 50)
# we need some unlabeled samples else trainOcc does not work...
# with the OCSVM these are not used during model building but only to
# derive the PU-performance metrics.
# note that you should probably not use the PU-performance metrics if you 
# do not have a set of unlabeled data where you expect a substantial 
# part of the unlabeled data to be negative
set.seed(seed)
idx.u <- sample(ncell(bananas$y), length(idx.p)+1)

x.p <- data.frame(bananas$x[][idx.p, ])
x <- bananas$x[][c(idx.p, idx.u), ]
y <- rep(c(1, 0), c(length(idx.p), length(idx.u)))
# with a few positive labeled data LOO cross validation is 
# recommended.
set.seed(seed)
index <- createFoldsPu(y, k=10)
sapply(index, function(i) table(y[i]))

tuneGrid = expand.grid(sigma=2^seq(-8, 7), 
                       nu=c(0.1)) # , 0.125, 0.15, 0.175, 0.2

model <- trainOcc(x=x, y=y, method="ocsvm", index=index, tuneGrid=tuneGrid)
model$finalModel

# -----------------------
# create a un-informative factor vector
# seed <- 1 # !!!
set.seed(seed)
x.p.f <- cbind(x.p, x3=factor(sample(1:2, nrow(x.p), replace=TRUE)))
mod <- consistent_ocsvm(x.p.f, sigma_thr=2.5, 
                        seed=seed, selectLastConsistent=FALSE,
                        refineGrid=T, verbose=F)
plot_consistent_ocsvm(mod)

m <- which(min(abs(tuneGrid$sigma-mod@kernelf@kpar$sigma))==abs(tuneGrid$sigma-mod@kernelf@kpar$sigma) & 
             tuneGrid$nu==0.1)
featurespace(update(model, modRow=m), 0, 
             main=paste(names(model$results[m, 1:3]), model$results[m, 1:3], 
                        collapse="|"))

# ----
m=m+1
featurespace(update(model, modRow=m), 0, 
             main=paste(names(model$results[m, 1:3]), model$results[m, 1:3], collapse="|"))
signif(model$results[m, ], 2)
