################################################################################
#' @name evaluateOneClass
#' 
#' @title Model evaluation / accuracy assessment for \code{oneClass} objects.
#'
#' @description Calculation of accuracy for specified thresholds.  
#' A slightly modified  version of the \code{evaluate} function from the package \code{dismo}
#' is called to perform the calculations.  
#'@details Only the final model is evaluated when \code{allModels} is \code{FALSE} 
#'and non of the arguments 
#'\code{modParam},\code{modRow}, or \code{modRank} given.
#'
#' @param x  an object of class \code{oneClass} or \code{iterative},  
#' @param y a vector of observed values. Values must be ... .
#' @param th ...
#' @param u the unlabeled data or predictor variables. Required only if 
#' \code{x} is a \code{train} object or if a model other than the final model is to be evaluated.
#' @param idx the index of y in x if y is a subset of x, or of x$pred if x is a 
#' oneClass-object.
#' @param positive the positive label in y
#' @param allModels \code{FALSE}, Set to \code{TRUE} if all models should be evaluated?
#' @param modParam data frame with the parameters of the model.
#' @param modRow the row or index of the model in the 
#' model-selection table. 
#' @param modRank the rank of the model after sorting by \code{by}.
#' this table is located in \code{x$results}/\code{x$results}).
#' @param by a character or character vector specifying by which columns to sort.
#' If \code{NULL} the performance metric is taken from the \code{train} object. 
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
#' # run oneClass 
#' oc <- oneClass(x=tr.x, y=puFactor(tr.y), 
#'                tuneGrid=expand.grid(sigma=c(0.1,1), ### not so large grid
#'                                     cNeg=2^seq(-5, 10, 3), 
#'                                     cMultiplier=2^seq(4, 15, 2)))
#' # evaluate the final model
#' ev <- evaluateOneClass(oc, y=te.y, u=te.x)
#' # besides the thresholds used, this is identical to: 
#' te.pred <- predict(oc, te.x)
#' ev <- evaluate(p=te.pred[te.y==1], a=te.pred[te.y!=1])
#' # evaluate several models
#' # e.g. evaluate models with a true positive rate (Tpr) higher than 0.8 and a 
#' # positive prediction probability (puPpv) small than 0.4
#' modRows <- which(oc$results$Tpr>=0.8 & oc$results$puPpv<0.4)
#' ev <- evaluateOneClass(oc, y=te.y, u=te.x, modRow=modRows)
#' # plot the pu-performance metric versus the maximum kappa 
#' evList <- print(ev)
#' plot(evList$puF, evList$mxK.K, xlab="puF", ylab="max. Kappa")
#' }
#' @export
evaluateOneClass <- function (x, y, th=NULL, u=NULL, idx=NULL, positive=NULL, 
                               allModels=FALSE, 
                               modParam=NULL, modRow=NULL, modRank=NULL, 
                               by=NULL, decreasing=TRUE, # allowParallel=FALSE,
                               ...) {
  if (!is.null(u))
    x$predUn <- predict(x, u)
  
  y <- puFactor(y, positive)
  
  if (is.null(idx))
    idx <- !logical(length(y))
  
  if (is.null(th))
    th <- as.vector(seq(min(x$predUn, na.rm=TRUE), max(x$predUn, na.rm=TRUE), length.out=100))
  
  
  ### evlauation of selected model, no u required
  if (allModels==FALSE & is.null(modParam) & is.null(modRow) & is.null(modRank) ) {
    #if (class(x)=='oneClass') {
      rtrn <- dismo::evaluate(p=x$predUn[idx][y=='pos'], a=x$predUn[idx][y!='pos'], tr=th)
    #} else if (class(x)=='numeric') {
    #  rtrn <- evaluate(p=x[y=='pos'], a=x[y!='pos'], tr=th)
    #}
    
    ### evlauation of one or more models other than the selected model. u required
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
    if ( is.null(u) )
      stop('Argument u must be given.')
    
    nModels <- length(models)
    evalParamList <- .paramList(x)[models, , drop=FALSE]
    rtrn <- vector("list", nModels)
    names(rtrn) <- paste("model.", models, sep="")
    
    
    cat(paste('Number of models to be evaluated: ', nModels, '\n', sep=""))
    if (class(u)=="ffraster") {                         # if u is a ffraster
      ############################################################################
      ### ffraster loop 
      for (i in seq(along.with=models)) {
        cat(paste(i, ".", sep=""))
        xU <- update(x, modParam=evalParamList[i, , drop=FALSE])
        #cat(paste("Predict test data.\n", sep=""))
        pred.i <- predict( u, xU$train, type="prob", index=idx )$p
        #cat(paste("Evaluate model.\n", sep=""))
        rtrn[[i]] <- dismo::evaluate(p=pred.i[y=='pos'], a=pred.i[!y!='pos'], tr=th)
      } 
    } else if (is.data.frame(u) | is.matrix(u)) {
      
      #       if (any(search()%in%"package:foreach") & allowParallel & nModels>1) {
      #         .parallelEvaluation <- function(i, x, u, y, evalParamList){
      #           browser()
      #           cat(paste(i, ".", sep=""))
      #           xU <- update(x, modParam=evalParamList[i, , drop=FALSE], u=u)
      #           #cat(paste("Evaluate model.\n", sep=""))
      #           ev <- evaluate(p=x$predUn[y], a=x$predUn[!y], th=x$plot$zDscrt)
      #           browser()
      #           return(ev)
      #         }
      #         rtrn <- foreach(i=1:nModels) %dopar% .parallelEvaluation(i, x, u, y, evalParamList)
      #       } else {
      for (i in seq(along.with=models)) {
        ############################################################################
        ### data.frame or matrix loop 
        cat(paste(i, ".", sep=""))
        xU <- update(x, modParam=evalParamList[i, , drop=FALSE], u=u)
        #cat(paste("Evaluate model.\n", sep=""))
        rtrn[[i]] <- dismo::evaluate(p=xU$predUn[y=='pos'], a=xU$predUn[y!='pos'], tr=th)
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
# evaluate.default <- function (x, y, tr=NULL, positive=1, ...) {
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
#   #} else if (!missing(x) & !missing(y)) {
#   #  rtrn <- myEvaluate(p=x[y==positive], a=x[y!=positive], tr, ...)
#   } else {
#     rtrn <- myEvaluate(x=x, ...)
#   }
#   return(rtrn)
# }