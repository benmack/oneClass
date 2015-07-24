#'@export
modelInfo <- 
  list(label="maxentExt", 
       library=c("dismo", "rJava"),
       loop=NULL,
       type="Classification",
       parameters=
         data.frame(parameter = c("fc", "beta"), 
                    class = rep("numeric", 1),
                    label = c("Beta")),
       grid = function(x, y, len = NULL) {
         grid <- expand.grid(fc=c("LC", "LCQ", "HC", "HQC", 
                                  "TC", "HQPTC"),
                             beta = 2^c(-1:5))
         colnames(grid) <- c("fc", "beta")
         return(grid)
       },
       fit = function(x, y, wts, param, lev, last, weights, classProbs, 
                      args.fit=NULL, path=NULL, deleteMaxentOutput=NULL, nPos=NULL, ...) {
         #cat(paste("Fit model with ", length(y), "samples.\n", sep=""))
         #--------------------------------------------------
         # FIT FUNCTIONS
         if (is.null(nPos))
           nPos <- nrow(x)
         .fc.default <- function(nPos) {
           if (nPos<10) {
             fc="LC"
           } else if (10<=nPos & nPos<80) {
             fc="LQC"
           } else if (80<=nPos) {
             fc="LQPTC"
           }
           return(fc)
         }
         .maxent.param2args <- function(fc, beta, args.fit) {
           ignore <- c("beta", "autofeature", "linear", "quadratic", 
                       "product", "threshold", "hinge")
           if (!is.null(args.fit)) {
             idx.ignore <- unlist(sapply(ignore, function(i) grep(i, args.fit)))
             if (length(idx.ignore)>0)
               args.fit <- args.fit[-idx.ignore]
           }
           args.fit.beta <- paste("betamultiplier=", beta, sep="")
           fcLong <- c("linear", "quadratic", "hinge", "product", "threshold")
           fcShort <- c("L", "Q", "H", "P", "T")
           fcSplit <- sapply(1:nchar(fc), function(i) substr(fc, i, i))
           args.fit.fc <- paste(fcLong[match(fcSplit, fcShort)], "true", sep="=")
           args.fit <- c(args.fit.beta, args.fit.fc, args.fit)
         }
         # FIT FUNCTIONS
         #--------------------------------------------------
         # --------------------------------------
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
           path <- paste(path, "/", "fc-", param$fc, "beta-", param$beta, 
                         "_uid-", uid, sep="")
         }
         # get an id 
         # --------------------------------------
         
         # --------------------------------------
         ### y vector for maxent
         y4maxent <- c(rep(1, sum(y=="pos")), rep(0, sum(y=="un")))
         
         # --------------------------------------
         ### model fit arguments
         if (param$fc=="D") {
           param$fc <- .fc.default(nP)
         }
         args.fit <- .maxent.param2args(param$fc, param$beta, args.fit)
         ### model fit arguments
         # --------------------------------------
         
         # --------------------------------------
         ### train
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
                                         "Maximum.training.sensitivity.plus.specificity.logistic.threshold"]
         predBin <- factor(pred >= defaultTh, 
                           levels=c(TRUE, FALSE), labels=c("pos", "un"), 
                           ordered=TRUE)
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
