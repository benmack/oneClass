################################################################################
#' @name evaluateOcc
#' 
#' @title Model evaluation / accuracy assessment for \code{\link{trainOcc}} objects.
#'
#' @description Calculation of accuracy for specified thresholds.  
#' \code{\link[dismo]{evaluate}} is called to perform the calculations.  
#' @details By defalut, only the final model is evaluated when \code{allModels} is \code{FALSE} 
#' and non of the arguments \code{modParam},\code{modRow}, or \code{modRank} given.
#'
#' @param x an object of class \code{\link{trainOcc}}
#' @param te.u the data to predict on.
#' @param te.y a vector of observed  positive/negative (presence/absence) values 
#' @param positive the positive label in te.y. if this is not given, the class with the lower frequency is assumed to be the positive class. 
#' @param th a vector of thresholds at which the model is evaluated
#' @param allModels logical, default is \code{FALSE}. Set to \code{TRUE} if all models should be evaluated.
#' @param modParam data frame with the parameters of the model to be evaluated
#' @param modRow the row of the model in the \code{x$results} table. 
#' @param modRank the rank of the model after sorting by \code{by}. 
#' @param by character. must be a metric available in the \code{x$results} table by which to sort. 
#' If \code{NULL} the performance metric is taken from the \code{train} object. see also \code{\link{sort.train}}
#' @param decreasing only when \code{modRank} is used. \code{TRUE} (default) to sort in decreasing order. Can be a vector if \code{by} is a vector.
#' @param ... arguments passed to \code{\link[dismo]{evaluate}} from the \code{dismo} package.
#' @return an object of class ModelEvaluation 
#' (see \code{\link{ModelEvaluation-class}})) 
#' or ModelSelectionEvaluation. The latter is a list with two elements, the 
#' first containing the model sleetion table and the second a list with the evaluation 
#' results, each of whith a \code{ModelEvaluation} object. 
#' The rows in model selection table correspond to the evaluation list elements.
#' @examples
#' \dontrun{
#' # get training and test data
#' data(bananas)
#' seed <- 123456
#' tr.x <- bananas$tr[, -1]
#' tr.y <- bananas$tr[, 1]
#' set.seed (seed)
#' te.i <- sample ( ncell (bananas$y), 1000 )
#' te.x <- extract (bananas$x, te.i)
#' te.y <- extract (bananas$y, te.i)
#' # run trainOcc 
#' oc <- trainOcc(x=tr.x, y=puFactor(tr.y), 
#'                tuneGrid=expand.grid(sigma=c(0.1,1), ### not so large grid
#'                                     cNeg=2^seq(-5, 10, 3), 
#'                                     cMultiplier=2^seq(4, 15, 2)))
#' # evaluate the final model
#' ev <- evaluateOcc(oc, y=te.y, te.u=te.x)
#' # besides the thresholds used, this is identical to: 
#' te.pred <- predict(oc, te.x)
#' ev <- evaluate(p=te.pred[te.y==1], a=te.pred[te.y!=1])
#' # evaluate several models
#' # e.g. evaluate models with a true positive rate (tpr) higher than 0.8 and a 
#' # positive prediction probability (ppp) small than 0.4
#' modRows <- which(oc$results$tpr>=0.8 & oc$results$ppp<0.4)
#' ev <- evaluateOcc(oc, y=te.y, te.u=te.x, modRow=modRows)
#' # plot the pu-performance metric versus the maximum kappa 
#' evList <- print(ev)
#' plot(evList$puF, evList$mxK.K, xlab="puF", ylab="max. Kappa")
#' }
#' @export
evaluateOcc <- function (x, te.u, te.y, positive=NULL, th=NULL, allModels=FALSE, 
                               modParam=NULL, modRow=NULL, modRank=NULL, 
                               by=NULL, decreasing=TRUE,
                               ...) {
  if (!is.null(te.u))
    x$predUn <- predict(x, te.u)
  
  te.y <- puFactor(te.y, positive)
  
  if (is.null(idx))
    idx <- !logical(length(te.y))
  
  if (is.null(th))
    th <- as.vector(seq(min(x$predUn, na.rm=TRUE), max(x$predUn, na.rm=TRUE), length.out=100))
  
  
  ### evlauation of selected model, no te.u required
  if (allModels==FALSE & is.null(modParam) & is.null(modRow) & is.null(modRank) ) {
    #if (class(x)=='trainOcc') {
      rtrn <- dismo::evaluate(p=x$predUn[idx][te.y=='pos'], a=x$predUn[idx][te.y!='pos'], tr=th)
    #} else if (class(x)=='numeric') {
    #  rtrn <- evaluate(p=x[te.y=='pos'], a=x[te.y!='pos'], tr=th)
    #}
    
    ### evlauation of one or more models other than the selected model. te.u required
  } else if (allModels==TRUE | (!is.null(modParam) | !is.null(modRow) | !is.null(modRank) ) ) {
    
    if ( allModels ) {
      models <- 1:nrow(x$results)
      
    } else {
      models <- c()
      if (!is.null(modParam)) {
        for (i in 1:nrow(modParam))
          models[i] <- modelPosition(x, modParam=modParam[i,,drop=FALSE])$row
      } else if (!is.null(modRow)) {
        models <- modRow
      } else if (!is.null(modRank)) {
        for (i in 1:length(modRank))
          models[i] <- modelPosition(x, modRank=modRank[i], by=by, decreasing=decreasing)$row
      }
    }
    if ( is.null(te.u) )
      stop('Argument te.u must be given.')
    
    nModels <- length(models)
    evalParamList <- .paramList(x)[models, , drop=FALSE]
    rtrn <- vector("list", nModels)
    names(rtrn) <- paste("model.", models, sep="")
    
    
    cat(paste('Number of models to be evaluated: ', nModels, '\n', sep=""))
    if (class(te.u)=="ffraster") {                         # if te.u is a ffraster
      ############################################################################
      ### ffraster loop 
      for (i in seq(along.with=models)) {
        cat(paste(i, ".", sep=""))
        xU <- update(x, modParam=evalParamList[i, , drop=FALSE])
        #cat(paste("Predict test data.\n", sep=""))
        pred.i <- predict( te.u, xU$train, type="prob", index=idx )$p
        #cat(paste("Evaluate model.\n", sep=""))
        rtrn[[i]] <- dismo::evaluate(p=pred.i[te.y=='pos'], a=pred.i[!te.y!='pos'], tr=th)
      } 
    } else if (is.data.frame(te.u) | is.matrix(te.u)) {
      
      #       if (any(search()%in%"package:foreach") & allowParallel & nModels>1) {
      #         .parallelEvaluation <- function(i, x, te.u, te.y, evalParamList){
      #           browser()
      #           cat(paste(i, ".", sep=""))
      #           xU <- update(x, modParam=evalParamList[i, , drop=FALSE], te.u=te.u)
      #           #cat(paste("Evaluate model.\n", sep=""))
      #           ev <- evaluate(p=x$predUn[te.y], a=x$predUn[!te.y], th=x$plot$zDscrt)
      #           browser()
      #           return(ev)
      #         }
      #         rtrn <- foreach(i=1:nModels) %dopar% .parallelEvaluation(i, x, te.u, te.y, evalParamList)
      #       } else {
      for (i in seq(along.with=models)) {
        ############################################################################
        ### data.frame or matrix loop 
        cat(paste(i, ".", sep=""))
        xU <- update(x, modParam=evalParamList[i, , drop=FALSE], te.u=te.u)
        #cat(paste("Evaluate model.\n", sep=""))
        rtrn[[i]] <- dismo::evaluate(p=xU$predUn[te.y=='pos'], a=xU$predUn[te.y!='pos'], tr=th)
        #}
      }
    }
    if (length(rtrn)>1) {
      rtrn <- list(train=x$results[models,], test=rtrn, 
                   model=x$modelInfo$label, metric=x$metric)
      class(rtrn) <- "ModelSelectionEvaluation"
    } else {
      rtrn <- rtrn[[1]]
    }
  }
  if (length(th)==1 & !is.null(th))
    rtrn <- evaluate(rtrn, th=th)
  return(rtrn)
}

### myEvaluate (see below), currently masked out
### if a large number of test samples is passed to dismo::evaluate an integer 
### overflow will make the calcualtion of some evaluation statistics impossible 
### (e.g. kappa). This problem is solved in the version below. 

# #' @rdname evaluate
# #' @export
# evaluate.default <- function (x, te.y, tr=NULL, positive=1, ...) {
#   #   myEvaluate is a copy of dismo::evaluate by with 6 lines exchanged:
#   #   in the following lines it differs by adding 'as.numeric' to avoid the 
#   #   'integer overflow' problem when large numbers of test data is used.
#   #   [...]
#   #   np <- as.numeric(length(p))
#   #   na <- as.numeric(length(a))
#   #   [...]
#   #   a = as.numeric(res[, 1])
#   #   b = as.numeric(res[, 2])
#   #   c = as.numeric(res[, 3])
#   #   d = as.numeric(res[, 4])
#   #   [...]
#   
#   myEvaluate <- function (p, a, model, x, tr, ...) 
#   {
#     if (!missing(x)) {
#       p <- predict(model, data.frame(extract(x, p)), ...)
#       a <- predict(model, data.frame(extract(x, a)), ...)
#     }
#     else if (is.vector(p) & is.vector(a)) {
#     }
#     else {
#       p <- predict(model, data.frame(p), ...)
#       a <- predict(model, data.frame(a), ...)
#     }
#     p <- na.omit(p)
#     a <- na.omit(a)
#     np <- as.numeric(length(p))
#     na <- as.numeric(length(a))
#     if (na == 0 | np == 0) {
#       stop("cannot evaluate a model without absence and presence data that are not NA")
#     }
#     if (missing(tr)) {
#       if (length(p) > 1000) {
#         tr <- as.vector(quantile(p, 0:1000/1000))
#       }
#       else {
#         tr <- p
#       }
#       if (length(a) > 1000) {
#         tr <- c(tr, as.vector(quantile(a, 0:1000/1000)))
#       }
#       else {
#         tr <- c(tr, a)
#       }
#       tr <- sort(unique(round(tr, 8)))
#       tr <- c(tr - 1e-04, tr[length(tr)] + c(0, 1e-04))
#     }
#     else {
#       tr <- sort(as.vector(tr))
#     }
#     N <- na + np
#     xc <- new("ModelEvaluation")
#     xc@presence = p
#     xc@absence = a
#     R <- sum(rank(c(p, a))[1:np]) - (np * (np + 1)/2)
#     xc@auc <- R/(na * np)
#     cr <- try(cor.test(c(p, a), c(rep(1, length(p)), rep(0, length(a)))), 
#               silent = TRUE)
#     if (class(cr) != "try-error") {
#       xc@cor <- cr$estimate
#       xc@pcor <- cr$p.value
#     }
#     res <- matrix(ncol = 4, nrow = length(tr))
#     colnames(res) <- c("tp", "fp", "fn", "tn")
#     xc@t <- tr
#     for (i in 1:length(tr)) {
#       res[i, 1] <- length(p[p >= tr[i]])
#       res[i, 2] <- length(a[a >= tr[i]])
#       res[i, 3] <- length(p[p < tr[i]])
#       res[i, 4] <- length(a[a < tr[i]])
#     }
#     xc@confusion = res
#     a = as.numeric(res[, 1])
#     b = as.numeric(res[, 2])
#     c = as.numeric(res[, 3])
#     d = as.numeric(res[, 4])
#     xc@np <- as.integer(np)
#     xc@na <- as.integer(na)
#     xc@prevalence = (a + c)/N
#     xc@ODP = (b + d)/N
#     xc@CCR = (a + d)/N
#     xc@TPR = a/(a + c)
#     xc@TNR = d/(b + d)
#     xc@FPR = b/(b + d)
#     xc@FNR = c/(a + c)
#     xc@PPP = a/(a + b)
#     xc@NPP = d/(c + d)
#     xc@MCR = (b + c)/N
#     xc@OR = (a * d)/(c * b)
#     prA = (a + d)/N
#     prY = (a + b)/N * (a + c)/N
#     prN = (c + d)/N * (b + d)/N
#     prE = prY + prN
#     xc@kappa = (prA - prE)/(1 - prE)
#     return(xc)
#   }
#   
#   if (missing(x)) {
#     rtrn <- myEvaluate(...)
#   #} else if (!missing(x) & !missing(te.y)) {
#   #  rtrn <- myEvaluate(p=x[te.y==positive], a=x[te.y!=positive], tr, ...)
#   } else {
#     rtrn <- myEvaluate(x=x, ...)
#   }
#   return(rtrn)
# }