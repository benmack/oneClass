context('Test MAXENT')

# require(devtools)
# require(testthat)

################################################################################
### method 
method <- 'maxent'
################################################################################
### data 
dt <- threeGaussians()
u <- dt$te[,-1]
seed <- 123456
tr.x <- dt$tr[, -1]
tr.y <- puFactor(dt$tr[, 1], 1)
set.seed (seed)
te.x <- dt$te[,-1]
te.y <- dt$te[,1]

## ----------------------------------------------------------------------------
### set up a small grid which leads to a over and underfitted 
# sigest(as.matrix(tr.x[tr.y=='pos', ]))
tuneGrid = ocsvm=expand.grid(beta=c(.0001, 100))

### change something in inst/models/ocsvm and run the following two lines to get the changes
### source("inst/models/parseModels.R")
### load_all()

oc <- trainOcc ( x = tr.x, y = tr.y, 
                 method=method, tuneGrid=tuneGrid)
oc
#plot(oc, plotType='level')
#plot(oc)

featurespace(update(oc, modRow=1), th=0)
featurespace(update(oc, modRow=2), th=0)

expect_true(oc$results$tpr[1]<oc$results$tpr[2])

# 
# 
# 
# trControl <- trainControl(method="cv", 
#                           number=10,
#                           summaryFunction = puSummary, 
#                           classProbs=TRUE, 
#                           savePredictions = TRUE,
#                           returnResamp = "all",
#                           verboseIter = FALSE, 
#                           allowParallel = TRUE)
# 
# modelInfo <- 
#   list(label="maxent", 
#        library=c("dismo", "rJava"),
#        loop=NULL,
#        type="Classification",
#        parameters=
#          data.frame(parameter = c("beta"), 
#                     class = rep("numeric", 1),
#                     label = c("Beta")),
#        grid = function(x, y, len = NULL) {
#          if (is.null(len)) {
#            grid <- expand.grid(beta = 2^c(-1:5))
#          } else {
#            grid <- expand.grid(beta = 2^c(-1:len))
#          }
#          colnames(grid) <- c("beta")
#          return(grid)
#        },
#        fit = function(x, y, wts, param, lev, last, weights, classProbs, 
#                       args.fit=NULL, path=NULL, deleteMaxentOutput=NULL, ...) {
#          #cat(paste("Fit model with ", length(y), "samples.\n", sep=""))
#          # get an id
#          if (!is.null(path)) {
#            dir.create(path, showWarnings=FALSE)
#            logfile <- paste(tempdir(), "\\maxentLogFile.txt", sep="")
#            if (file.exists(logfile)) {
#              tbl <- read.table(logfile)
#              write.table(c(tbl[1]+1, tbl), logfile, 
#                          row.names=FALSE, col.names=FALSE)
#              uid <- tbl[1]
#            } else {
#              tbl <- data.frame(id=1)
#              write.table(tbl, file=logfile, 
#                          row.names=FALSE, col.names=FALSE)
#              uid <- tbl[1]
#            }
#            path <- paste(path, "/", "beta-", param, "_uid-", uid, sep="")
#          }
#          ### y vector for maxent
#          y4maxent <- c(rep(1, sum(y=="pos")), rep(0, sum(y=="un")))
#          
#          ### model fit arguments
#          if (is.null(args.fit)) {
#            args.fit <- c(paste("betamultiplier=", param$beta, sep=""))
#          } else {
#            ### check if beta is given and ignore it if this is the case
#            removeBetaArg <- grep("betamultiplier", args.fit)
#            if (length(removeBetaArg)>0)
#              args.fit <- args.fit[-removeBetaArg]
#            args.fit <- c(paste("betamultiplier=", param$beta, sep=""), args.fit)
#          }
#          
#          ### set up the arguments
#          rJava::.jinit()
#          if(is.null(path)) {
#            modelFit <- dismo::maxent(x = x, p = y4maxent, 
#                                      args=args.fit)
#            if (is.null(deleteMaxentOutput)) {
#              unlink(modelFit@path)
#            } else if (deleteMaxentOutput) {
#              unlink(modelFit@path)
#            }
#          } else {
#            modelFit <- dismo::maxent(x = x, p = y4maxent, 
#                                      args=args.fit, path=path)
#            if (!is.null(deleteMaxentOutput)) {
#              if (deleteMaxentOutput)
#                unlink(modelFit@path)
#            }
#          }
#          ### unlink
#          return(modelFit)
#        }, 
#        predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
#          pred <- predict(modelFit, newdata)
#          defaultTh <- modelFit@results[rownames(modelFit@results)==
#                                          "Equal.training.sensitivity.and.specificity.logistic.threshold"]
#          predBin <- factor(pred >= defaultTh, 
#                            levels=c(TRUE, FALSE), labels=c("pos", "un"), 
#                            ordered=TRUE)
#          predBin
#        },
#        ### prob
#        prob=function(modelFit, newdata, preProc = NULL, submodels = NULL, 
#                      args.predict=NULL) {
#          
#          prob <- predict(modelFit, newdata)
#          prob <- cbind(prob, prob)
#          colnames(prob) <- c("pos", "un")
#          prob
#          
#        },
#        ### predictors
#        predictors=NULL,
#        ### tags
#        tags=NULL,
#        ### levels
#        levels=function(x) c("un", "pos") ,
#        ### sort
#        sort=function(x) x[order(-x$beta), ],
#        ### varImp
#        varimp=NULL)  
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 
# tune <- train(x=tr.x, y=tr.y,
#               method = modelInfo, 
#               metric = "puAuc", 
#               trControl = trControl)
# 
# rJava::.jinit()
# modelFit <- dismo::maxent(x = tr.x, p = rep(c(1,0), c(20,100)) )
# 
