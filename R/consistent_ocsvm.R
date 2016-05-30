#' Consistency based tuning OCSVM
#'
#' @description The complexity of model is increased until the classifier becomes inconsistent on the data. 
#' Thus, the most complex classifier is selected that can still be trained reliably on the data.
#' This is a translation of the code found in the \code{inconsistency_occ} function of the MATLAB \code{dd_toolbox} (http://prlab.tudelft.nl/david-tax/dd_tools.html). 
#' For more information, see also Tax & Mueller (2004). 
#' 
#' @param x Data matrix with positive data only. Observations/features in rows/columns.
#' @param target_fnr Target false positive rate and the nu parameter for the one-svc
#' @param sigmas  Vector of inverse kernel widths for the Radial Basis kernel function ordered from low (simple model) to high (complex model) values. 
#' @param k Number of cross-validation folds used to estimate the empirical false positive rate.
#' @param sigma_thr consistency threshold in terms of sigma-bounds.
#' @param seed A seed value for the cross-validation split.
#' @param selectLastConsistent If TRUE the model just before passing the consistency threshold is selected. If FALSE the one after the threshold. Defaults to FALSE as this is the default in the dd_toolbox.
#' @param oneMoreComplexity Calculate the false positive rate for one more model after the consistency threshold has been passed. Might be interesting for plotting.
#' @param refineGrid Re-selection with a grid refined between the sigma before and after the selected one? Defaults to FALSE
#' @param refineFun a function that takes a numeric vector of length two, i.e. the new minimum and maximum of the sigma range and creates new values, usually between the two.
#' Defaults to \\code{function(rng) seq(rng[1], rng[2], length.out=10))}. 
#' @param verbose Print progress info? Defaults to TRUE. 
#' @return Trained OCSVM model. 
#' @export
#' @references
#' Tax, D.M.J. & Mueller, K. R., 2004. A consistency-based model selection for one-class classification. In Proceedings of the 17th International Conference on Pattern Recognition, 2004. ICPR 2004. IEEE, pp. 363 to 366 Vol.3. Available at: http://ieeexplore.ieee.org/lpdocs/epic03/wrapper.htm?arnumber=1334542.
#' Tax, D.M.J., 2015. DDtools, the Data Description Toolbox for Matlab.
#' @examples
#' data(bananas)
#' idx.p <- sample(which(bananas$y[]==1), 50)
#' x.p <- bananas$x[][idx.p, ]
#' mod <- consistent_ocsvm(x.p, sigma_thr=2)
#' # plot_consistent_ocsvm(mod)
consistent_ocsvm <- function(x, target_fnr=.1, 
                             sigmas=2^seq(-8, 7), # following Hsu 
                             k=10,
                             sigma_thr=2, 
                             seed=NULL,
                             selectLastConsistent=FALSE,
                             oneMoreComplexity=TRUE,
                             refineGrid=FALSE,
                             refineFun=function(rng) seq(rng[1], rng[2], 
                                                         length.out=10),
                             verbose=TRUE) {
  
  mod <- .consistent_ocsvm(x, target_fnr=target_fnr, 
                           sigmas=sigmas, # following Hsu 
                           k=k,
                           sigma_thr=sigma_thr, 
                           seed=seed,
                           selectLastConsistent=selectLastConsistent,
                           oneMoreComplexity=oneMoreComplexity,
                           verbose=verbose)
  if (refineGrid) {
    est_fnr <- attr(mod, "est_fnr")
    idx_sel <- which(est_fnr==mod@kernelf@kpar$sigma)
    new_rng <- c(est_fnr$sigma[idx_sel-1], est_fnr$sigma[idx_sel+1])
    new_sigmas <- refineFun(new_rng)
    # we do not do that since then we do not need to care
    # if the first new sigma leads to an inconistent model
    # new_sigmas <- new_sigmas[!new_sigmas %in% sigmas]
    mod_new <- .consistent_ocsvm(x, target_fnr=target_fnr,
                                 sigmas=new_sigmas, # NEW GRID
                                 k=k,
                                 sigma_thr=sigma_thr, 
                                 seed=seed,
                                 selectLastConsistent=selectLastConsistent,
                                 oneMoreComplexity=oneMoreComplexity,
                                 verbose=verbose)
    est_fnr_new <- attr(mod_new, "est_fnr")
    est_fnr <- rbind(est_fnr, est_fnr_new[!is.na(est_fnr_new$est_fnr), ])
    est_fnr <- est_fnr[order(est_fnr$sigma), ]
    est_fnr <- est_fnr[!duplicated(est_fnr), ]
    attr(mod_new, "est_fnr") <- est_fnr
    attr(mod_new, "sigma_selected") <- c(attr(mod_new, "sigma_selected"), 
                                         attr(mod, "sigma_selected"))
    
  return(mod_new)
  }
  return(mod)
}

.consistent_ocsvm <- function(x, target_fnr=.1, 
                              sigmas=2^seq(-8, 7), # following Hsu 
                              k=10,
                              sigma_thr=2, 
                              seed=NULL,
                              selectLastConsistent=FALSE,
                              oneMoreComplexity=TRUE, 
                              verbose=TRUE) {
  
  fracrej=target_fnr
  range=sigmas
  nrbags=k; rm(k)
  
  if ("DUMMY_RESPONSE_DUMMY" %in% colnames(x))
    stop("x cannot have a variable called DUMMY_RESPONSE_DUMMY.")
  x <- cbind(DUMMY_RESPONSE_DUMMY=as.factor(rep(1, nrow(x))), x)
  
  myksvm <- function(x, nu, sigma) {
    #     ksvm(DUMMY_RESPONSE_DUMMY~., # looks odd but with a formula we can pass factors!
    #          data=x, type="one-svc", kernel = "rbfdot",
    #          kpar = list(sigma=sigma),
    #          nu = fracrej, cross=0)
    ksvm(DUMMY_RESPONSE_DUMMY~., data=x, type="one-svc",
         kernel = "rbfdot",
         kpar = list(sigma = sigma), 
         nu = nu, cross=0)
  }
  
  get_est_fpr <- 
    function (x, fracrej, sigma, index, index.te, nrbags) {
      err <- vector("numeric", nrbags)
      for (i in 1:nrbags) {
        xtr <- x[index[[i]],]
        xte <- x[index.te[[i]],]
        mod.i <-  myksvm(xtr, nu=fracrej, sigma=sigma)
        pred.i <- predict(mod.i, xte[, -1])
        err[i] <- 1-mean(pred.i)
      }
      return(err)
    }
  
  nrrange = length(range);
  if (nrrange<2)
    stop('Expecting a range of param. values (from simple to complex classifier)');
  
  if (length(nrbags) == 1) {
    if (!is.null(seed))
      set.seed(seed)
    index <- createFoldsPu(rep(1, nrow(x)), k=nrbags, positive=1)
  }
  # } else {
  # assuming a list with training indices
  # }
  index.te <- lapply(index, function(i) which(!1:nrow(x) %in% i))
  
  # Setup the consistency threshold, say the three sigma bound:
  # DXD still a magic parameter!
  nrx = nrow(x)
  err_thr = fracrej + (sigma_thr*sqrt(fracrej*(1-fracrej)/nrx))
  # AI!---------------^
  
  # Train the most simple classifier:
  k = 1
  fracout <- rep(NA, length(sigmas))
  I = nrbags
  err <- get_est_fpr(x=x, fracrej=fracrej, sigma=range[k], 
                     index=index, index.te=index.te, nrbags=nrbags)
  
  # This one should at least satisfy the bound, else the model is already
  # too complex?!
  if (mean(err)>err_thr) {
    stop("The most simple classifier is already inconsistent!")
  }
  fracout[k] <- mean(err)
  
  # Go through the other parameter settings until it becomes inconsistent:
  while ((k<nrrange) & (fracout[k] < err_thr)) {
    if (verbose)
      cat("\nest_fnr  < th :", fracout[k], "  < ", 
          err_thr, "@", k, "(sigma=", range[k], ")")
    k = k+1;
    I = nrbags;
    err <- get_est_fpr(x=x, fracrej=fracrej, sigma=range[k], 
                       index=index, index.te=index.te, nrbags=nrbags)
    fracout[k] = mean(err)
  }
  if(selectLastConsistent) {
    k.sel <- k-1
    if (verbose)
      cat("  SELECTED! \nest_fnr => th :", fracout[k], " => ", 
          err_thr, "@", k, "(sigma=", range[k], ")\n")
  } else {
    k.sel <- k
    if (verbose)
      cat("\nest_fnr => th :", fracout[k], " => ", 
          err_thr, "@", k, "(sigma=", range[k], ")  SELECTED!\n")
  }
  
  if (oneMoreComplexity) {
    k = k+1;
    I = nrbags;
    err <- get_est_fpr(x=x, fracrej=fracrej, sigma=range[k], 
                       index=index, index.te=index.te, nrbags=nrbags)
    fracout[k] = mean(err)
    if (verbose)
      cat("est_fnr => th :", fracout[k], " => ", 
          err_thr, "@", k, "(sigma=", range[k], ")\n")
    k=k-1
  }
  
  # So, the final classifier becomes:
  mod <- myksvm(x, nu=fracrej, sigma=range[k.sel])
  attr(mod, "est_fnr") <- data.frame(sigma=sigmas,
                                     est_fnr=fracout)
  attr(mod, "consistency_threshold") <- err_thr
  attr(mod, "sigma_selected") <- range[k.sel]
  return(mod)
}

#' Plot model complexity (sigma) versus FNR of consistency tuned OCSVM 
#'
#' @param x object returned by \code{\link{consistent_ocsvm}}
#'
#' @export
#'
#' @examples
#' #' data(bananas)
#' idx.p <- sample(which(bananas$y[]==1), 50)
#' x.p <- bananas$x[][idx.p, ]
#' mod <- consistent_ocsvm(x.p, sigma_thr=2)
#' # plot_consistent_ocsvm(mod)
plot_consistent_ocsvm <- function(x) {
  est_fnr <- attr(x, "est_fnr")
  plot(log(est_fnr$sigma), est_fnr$est_fnr, xlab="log(sigma)", ylab="FPR",
       main = paste0("Consistency threshold = ", signif(attr(x, "consistency_threshold"), 2), "\nsigma = ", signif(x@kernelf@kpar$sigma, 2)))
  abline(h=attr(x, "consistency_threshold"))
  abline(v=log(attr(x, "sigma_selected")))
}