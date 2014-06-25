modelInfo <- 
  list(label="maxent", 
       library=c("dismo", "rJava"),
       loop=NULL,
       type="Classification",
       parameters=
         data.frame(parameter = c("beta"), 
                                        class = rep("numeric", 1),
                                        label = c("Beta")),
       grid = function(x, y, len = NULL) {
         if (is.null(len)) {
           grid <- expand.grid(beta = 2^c(-1:5))
         } else {
           grid <- expand.grid(beta = 2^c(-1:len))
         }
         colnames(grid) <- c("beta")
         return(grid)
       },
       fit = function(x, y, wts, param, lev, last, weights, classProbs, 
                      args.fit=NULL, path=NULL, deleteMaxentOutput=NULL, ...) {
         #cat(paste("Fit model with ", length(y), "samples.\n", sep=""))
         # get an id
         if (!is.null(path)) {
           dir.create(path, showWarnings=FALSE)
           logfile <- paste(tempdir(), "\\maxentLogFile.txt", sep="")
           if (file.exists(logfile)) {
             tbl <- read.table(logfile)
             write.table(c(tbl[1]+1, tbl), logfile, 
                         row.names=FALSE, col.names=FALSE)
             uid <- tbl[1]
           } else {
             tbl <- data.frame(id=1)
             write.table(tbl, file=logfile, 
                         row.names=FALSE, col.names=FALSE)
             uid <- tbl[1]
           }
           path <- paste(path, "/", "beta-", param, "_uid-", uid, sep="")
         }
         ### y vector for maxent
         y4maxent <- c(rep(1, sum(y=="pos")), rep(0, sum(y=="un")))
         
         ### model fit arguments
         if (is.null(args.fit)) {
           args.fit <- c(paste("betamultiplier=", param$beta, sep=""))
         } else {
           ### check if beta is given and ignore it if this is the case
           removeBetaArg <- grep("betamultiplier", args.fit)
           if (length(removeBetaArg)>0)
             args.fit <- args.fit[-removeBetaArg]
           args.fit <- c(paste("betamultiplier=", param$beta, sep=""), args.fit)
         }
         
         ### set up the arguments
         rJava::.jinit()
         if(is.null(path)) {
           modelFit <- dismo::maxent(x = x, p = y4maxent, 
                                     args=args.fit)
           if (is.null(deleteMaxentOutput)) {
             unlink(modelFit@path)
           } else if (deleteMaxentOutput) {
             unlink(modelFit@path)
           }
         } else {
           modelFit <- dismo::maxent(x = x, p = y4maxent, 
                                     args=args.fit, path=path)
           if (!is.null(deleteMaxentOutput)) {
             if (deleteMaxentOutput)
               unlink(modelFit@path)
           }
         }
         ### unlink
         return(modelFit)
       }, 
       predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
         pred <- predict(modelFit, newdata)
         defaultTh <- modelFit@results[rownames(modelFit@results)==
                                         "Equal.training.sensitivity.and.specificity.logistic.threshold"]
         predBin <- factor(pred >= defaultTh, levels=c(TRUE, FALSE), labels=c("un", "pos"), ordered=TRUE)
         predBin
       },
       ### prob
       prob=function(modelFit, newdata, preProc = NULL, submodels = NULL, 
                     args.predict=NULL) {
         
         prob <- predict(modelFit, newdata)
         prob <- cbind(prob, prob)
         colnames(prob) <- c("pos", "un")
         prob
         
       },
       ### predictors
       predictors=NULL,
       ### tags
       tags=NULL,
       ### levels
       levels=function(x) c("un", "pos") ,
       ### sort
       sort=function(x) x[order(-x$beta), ],
       ### varImp
       varimp=NULL)  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
