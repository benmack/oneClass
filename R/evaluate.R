################################################################################
#' @name evaluate
#' 
#' @title Model evaluation / accuracy assessment
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
#' @param idx the index of y in x if y is a subset of x, or of x$pred if x is a 
#' oneClass-object.
#' @param u the unlabeled data or predictor variables. Required only if 
#' \code{x} is a \code{train} object or if a model other than the final model is to be evaluated.
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
#' ### to do
#' @rdname evaluate
#' @export evaluate
evaluate <- function(x, y, ...) UseMethod("evaluate") 


#' @rdname evaluate ModelEvaluation
#' @export 
evaluate.ModelEvaluation <- function (x, th, ...) {
  
  ################################################################################
  .indexOfThInEval <- function (x, th) {
    if ( is.numeric(th) ) {
      ### get nearest threshold
      th.idx <- which.min( abs(x@t-th) )
    } else if ( is.character(th) ) {
      dummy <- strsplit(th, split="[.]")[[1]]
      prs <- parse( text = 
                      paste("which.", dummy[1], "(x@", dummy[2], ")", sep="") )
      th.idx <- eval(prs)
    }
    return(th.idx)
  }
  
  .getEvaluation <- function (x, th.idx) {
    
    thDepSlots <- c("t", "prevalence", "ODP", "CCR", "TPR", 
                    "TNR", "FPR", "FNR", "PPP", "NPP", "MCR", "OR", "kappa")
    confmat <- matrix(x@confusion[th.idx,],2,2, byrow=TRUE)
    colnames(confmat) <- c("+ (Test)", "- (Test)")
    rownames(confmat) <- c("+ (Pred)", "- (Pred)")
    
    rtrn <- list(np=x@np, 
                 na=x@na, 
                 confusion=confmat, 
                 thIndependent=data.frame(auc=x@auc, 
                                          pauc=ifelse(length(x@pauc)==1, x@pauc, NA), 
                                          cor=x@cor, pcor=x@pcor),
                 c(auc=x@auc, pauc=x@pauc, cor=x@cor, 
                   pcor=x@pcor), 
                 thDependent=c() )
    
    dummy <- data.frame(matrix(0, 1, length(thDepSlots)))
    colnames(dummy) <- thDepSlots
    
    for(i in seq(along.with=thDepSlots)) {
      slotName <- thDepSlots[i]
      expr <- parse(text=paste("x@", slotName, "[th.idx]", sep="")) 
      dummy[, slotName] <- eval(expr)
    }
    rtrn$thDependent <- dummy
    return(rtrn)
  }
  
  if ( class(x)=="ModelEvaluation" ) {
    th.idx <- .indexOfThInEval(x, th)
    rtrn <- .getEvaluation(x, th.idx)
    rtrn$th.call <- th
    rtrn$t.idx <- th.idx
    class(rtrn) <- 'ModelEvaluationAtTh'
  } else if ( class(x)== "ModelSelectionEvaluation" ) {
    rtrn <- list()
    for (i in seq(along.with=x$test)) {
      th.idx <- .indexOfThInEval(x$test[[i]], th)
      rtrn.i <- .getEvaluation(x$test[[i]], th.idx)
      rtrn.i$th.call <- th
      rtrn.i$t.idx <- th.idx
      class(rtrn.i) <- 'ModelEvaluationAtTh'
      rtrn[[i]] <- rtrn.i
    }
    names(rtrn) <- names(x$test)
    class(rtrn) <- 'ModelSelectionEvaluationAtTh'
  } else {
    stop("x must be an object of class 'ModelEvaluation' or 'ModelSelectionEvaluation'.")
  }
  
  return(rtrn)
}

#' @rdname evaluate
#' @export 
evaluate.oneClass <- function (x, y, th=NULL, idx=NULL, u=NULL, positive=NULL, 
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
    rtrn <- evaluate(p=x$predUn[idx][y=='pos'], a=x$predUn[idx][y!='pos'], tr=th)
    
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
        rtrn[[i]] <- evaluate(p=pred.i[y=='pos'], a=pred.i[!y!='pos'], tr=th)
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
        rtrn[[i]] <- evaluate(p=xU$predUn[y=='pos'], a=xU$predUn[y!='pos'], tr=th)
        #}
      }
    }
    if (length(rtrn)>1) {
      rtrn <- list(train=x$results[models,], test=rtrn, model=x$modelInfo$label)
      class(rtrn) <- "ModelSelectionEvaluation"
    } else {
      rtrn <- rtrn[[1]]
    }
  }
  
  if (length(th)==1 & !is.null(th))
    rtrn <- evaluate(rtrn, th=th)
    
  
  return(rtrn)
}

#' @rdname evaluate
#' @export
evaluate.default <- function (x, y, ...) {
  #   myEvaluate is a copy of dismo::evaluate by with 6 lines exchanged:
  #   in the following lines it differs by adding 'as.numeric' to avoid the 
  #   'integer overflow' problem when large numbers of test data is used.
  #   [...]
  #   np <- as.numeric(length(p))
  #   na <- as.numeric(length(a))
  #   [...]
  #   a = as.numeric(res[, 1])
  #   b = as.numeric(res[, 2])
  #   c = as.numeric(res[, 3])
  #   d = as.numeric(res[, 4])
  #   [...]
  myEvaluate <- function (p, a, model, x, tr, ...) 
  {
    if (!missing(x)) {
      p <- predict(model, data.frame(extract(x, p)), ...)
      a <- predict(model, data.frame(extract(x, a)), ...)
    }
    else if (is.vector(p) & is.vector(a)) {
    }
    else {
      p <- predict(model, data.frame(p), ...)
      a <- predict(model, data.frame(a), ...)
    }
    p <- na.omit(p)
    a <- na.omit(a)
    np <- as.numeric(length(p))
    na <- as.numeric(length(a))
    if (na == 0 | np == 0) {
      stop("cannot evaluate a model without absence and presence data that are not NA")
    }
    if (missing(tr)) {
      if (length(p) > 1000) {
        tr <- as.vector(quantile(p, 0:1000/1000))
      }
      else {
        tr <- p
      }
      if (length(a) > 1000) {
        tr <- c(tr, as.vector(quantile(a, 0:1000/1000)))
      }
      else {
        tr <- c(tr, a)
      }
      tr <- sort(unique(round(tr, 8)))
      tr <- c(tr - 1e-04, tr[length(tr)] + c(0, 1e-04))
    }
    else {
      tr <- sort(as.vector(tr))
    }
    N <- na + np
    xc <- new("ModelEvaluation")
    xc@presence = p
    xc@absence = a
    R <- sum(rank(c(p, a))[1:np]) - (np * (np + 1)/2)
    xc@auc <- R/(na * np)
    cr <- try(cor.test(c(p, a), c(rep(1, length(p)), rep(0, length(a)))), 
              silent = TRUE)
    if (class(cr) != "try-error") {
      xc@cor <- cr$estimate
      xc@pcor <- cr$p.value
    }
    res <- matrix(ncol = 4, nrow = length(tr))
    colnames(res) <- c("tp", "fp", "fn", "tn")
    xc@t <- tr
    for (i in 1:length(tr)) {
      res[i, 1] <- length(p[p >= tr[i]])
      res[i, 2] <- length(a[a >= tr[i]])
      res[i, 3] <- length(p[p < tr[i]])
      res[i, 4] <- length(a[a < tr[i]])
    }
    xc@confusion = res
    a = as.numeric(res[, 1])
    b = as.numeric(res[, 2])
    c = as.numeric(res[, 3])
    d = as.numeric(res[, 4])
    xc@np <- as.integer(np)
    xc@na <- as.integer(na)
    xc@prevalence = (a + c)/N
    xc@ODP = (b + d)/N
    xc@CCR = (a + d)/N
    xc@TPR = a/(a + c)
    xc@TNR = d/(b + d)
    xc@FPR = b/(b + d)
    xc@FNR = c/(a + c)
    xc@PPP = a/(a + b)
    xc@NPP = d/(c + d)
    xc@MCR = (b + c)/N
    xc@OR = (a * d)/(c * b)
    prA = (a + d)/N
    prY = (a + b)/N * (a + c)/N
    prN = (c + d)/N * (b + d)/N
    prE = prY + prN
    xc@kappa = (prA - prE)/(1 - prE)
    return(xc)
  }
  
  
  if (missing(x)) {
    rtrn <- myEvaluate(...)
  } else {
    rtrn <- myEvaluate(x=x, ...)
  }
  return(rtrn)
}