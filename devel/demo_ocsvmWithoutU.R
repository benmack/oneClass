require(devtools)

require(doParallel)
require(kernlab)

require(oneClass)

nCores <- detectCores()
cl <- makeCluster(nCores)
registerDoParallel(cl)


load_all('.')
data(bananas)
table(bananas$tr[, 1])

# we need some unlabeled samples else trainOcc does not work...
bananas$tr <- bananas$tr[1:41, ]

# with a few positive labeled data LOO cross validation is 
# recommended.
index <- createFoldsPu(bananas$tr[, 1], k=20)

P <- as.matrix(bananas$tr[bananas$tr[, 1]==1, -1])
sigest(P)
c(min(sapply(1:100, function(i) range(sigest(P)))[1,]),
  max(sapply(1:100, function(i) range(sigest(P)))[2,]))

tuneGrid = expand.grid(sigma=c(0.1, .25, .5, .75, 1, 2.5, 5), 
                       nu=c(0.01, 0.05, 0.1)) # , 0.125, 0.15, 0.175, 0.2

tuneGrid = expand.grid(sigma=c(0.1, .25, .5, .75, 1, 2.5, 5), 
                       nu=c(0.1)) # , 0.125, 0.15, 0.175, 0.2

model <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], 
                  method="ocsvm", index=index, tuneGrid=tuneGrid)

m=0
m=m+1
featurespace(update(model, modRow=m), 0, 
             main=paste(names(model$results[m, 1:3]), model$results[m, 1:3], collapse="|"))
signif(model$results[m, ], 2)

# ----
# inconsistency tuning by Tax $ Müller (2004)
et <- 0.1 # user defined error (false negative rate) on the target class
M <- nrow(P)

#
inconsistency_criteria <- et + ( 2 * sqrt(M*et*(1-et)) )




E_ethat <- M*et
V_ethat <- M*et*(1-et)
