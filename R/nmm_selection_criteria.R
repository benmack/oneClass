#' @title nmm_selection_criteria
#' 
#' @description runs a couple of checks which can be used in order to select a model. 
#' 
#' @param x an object of class \code{trainOcc}
#' @param un ...
#' @param nmm the normal mixture (density) models (see also \code{\link[Mclust]{densityMclust}}
#' @param modRow the model in \code{x} used to predict the data with which \code{nmm} has been estimated. If \code{null} the \code{finalModel} (\code{x$finalModel}) is used.
#' @param fnamePlot if given the diagnostic is saved to the specified filename (defualt is \code{NULL})
#' @export
nmm_selection_criteria <- function(x, un=NULL, nmm=NULL, modRow=NULL, predU=NULL, predFromNumm=FALSE, fnamePlot=NULL) {
 
  # allow 
  #  #' @param aggregateHop should the hold-out predictions be aggregated (defualt is \code{TRUE})
  aggregateHop=TRUE
  
  ##############################################################################
  ### functions 
  
  ### --------------------------------------------------------------------------
  ### theoretical positive distribution
  getTheo <- function (nmm) {
    idx <- which.max(nmm$parameters$mean)
    ans <- length(nmm$parameters$variance$sigmasq)
    idx.var <- ifelse(ans==1, 1,  ans)
    theo <- list(mean = nmm$parameters$mean[idx],
                 var = nmm$parameters$variance$sigmasq[idx.var],  ### Varianz
                 sd = sqrt(nmm$parameters$variance$sigmasq[idx.var]))
    return(theo) }
  # theo <- getTheo(nmm)
  
  ### --------------------------------------------------------------------------
  ### kruskal wallis test
  myKruskal <- function (pos, un) {
    tst <- kruskal.test(x=c(pos, un), rep(c(1, 0), c(length(pos), length(un)) ) )
    return(c(s.kruskal=tst$statistic[[1]], p.kruskal=tst$p.value[[1]]))
  }
  # myKruskal(pos, un)
  
  ### --------------------------------------------------------------------------
  ### kolmogorov-smirnoff test
  myKs <- function(pos, theo) {
    tst <- ks.test(pos, "pnorm", mean = theo$mean, sd = theo$sd)
    return(c(s.ks=tst$statistic[[1]], p.ks=tst$p.value))
  }
  # myKs(pos, theo)
  
  ### --------------------------------------------------------------------------
  ### z test 
  myZ <- function (pos, theo) {
    tst <- (mean(pos)-theo$mean)/(theo$sd)
    p.z <- pnorm(abs(tst)) - pnorm(-abs(tst))
    # ifelse(pnorm(myz) < 0.025 | pnorm(myz) > 0.975, "Test signifikant für alpha=0.05", "Sorry, not significant")
    return(c(p.z=p.z[[1]]))
  }
  # myZ(pos, theo)
  # mytest <- t.test(pos,  alternative = "two.sided", mu = theo$mean,
  #                 paired = FALSE, var.equal = FALSE, conf.level = 0.95)
  
  ### --------------------------------------------------------------------------
  ### chi-square test
  getChi <- function(pos, theo) max((var(pos)/sqrt(length(pos)))/theo$var, theo$var/(var(pos)/sqrt(length(pos))))  
  myChisq <- function(pos, theo) {
    chi <- getChi(pos, theo)
    p <- pchisq(chi, df=length(pos)-1, lower.tail = FALSE, log.p = FALSE)
    return(c(s.chi=chi,p.chi=p))
  }
  # myChisq(pos, theo)
  
  ### --------------------------------------------------------------------------
  ### error/overlap between the (assumed) positive and all other components
  ### returns the min. error threshold
  threshold_nmm(nmm)
  
  ### returns the minimum density 
  min_f1f2 <- function(newdata, model) {
    idx <- which.max(nmm$parameters$mean)
    d.mat <- predict(model, newdata, what="cdens")
    pro.mat <- matrix(rep(model$parameters$pro), length(newdata), model$G, byrow=TRUE)
    scaled.d <- d.mat*pro.mat
    rtrn <- pmin(rowSums(scaled.d[, -idx, drop=FALSE]), scaled.d[, idx])
    return(rtrn)
  }
  
  getOverlap <- function(nmm, theo, th=NULL) {
    if (is.null(th)) {
      rng <- range(qnorm(c(.01, .99), theo$mean, theo$sd))
      olp <- integrate(min_f1f2, lower=rng[1], upper=rng[2], 
                       model=nmm, subdivisions=100L)
      return(olp)
    } else {
      threshold_nmm(unSub_nmm[[6]])
      rng <- range(qnorm(c(.01, .99), theo$mean, theo$sd))
      threshold_nmm(nmm, seq(rng[1], rng[2], length.out=101))
    }
    
  }
  
  ### --------------------------------------------------------------------------
  ### error/overlap between the theoretical distribution and the empirical
  min_f1f2_PP <- function(newdata, theo, empi) {
    pmin(dnorm(newdata, theo$mean, theo$sd), 
         dnorm(newdata, empi$mean, empi$sd))
  }
  getOverlap_PP <- function(theo, empi) {
    rng <- range(c(qnorm(c(.01, .99), theo$mean, theo$sd), 
                   qnorm(c(.01, .99), empi$mean, empi$sd)))
    integrate(min_f1f2_PP, lower=rng[1], upper=rng[2], theo=theo, empi=empi, subdivisions=1000L)
  }
  
  ### --------------------------------------------------------------------------
  ### 
  plot_theoEmpi <- function(theo, empi) {
    rng <- range(c(qnorm(c(.01, .99), theo$mean, theo$sd), 
                   qnorm(c(.01, .99), empi$mean, empi$sd)))
    x <- seq(rng[1], rng[2], length.out=50)
    plot(x, dnorm(x, theo$mean, theo$sd), xlab="predictive value", ylab="density")
    points(x, dnorm(x, empi$mean, empi$sd), pch=4)
    legend("topright", c("theoretical from nmm", "est. from P"), pch=c(1,4))
  }
  
  ### --------------------------------------------------------------------------
  ### 
  diagnosticPlot <- function(x, nmm, tsts, empi=NULL, ...) {
    
    rng <- nmm$range
    input <- seq(rng[1], rng[2], length.out=101)
    ys <- min_f1f2(input, nmm)
    xs <- c(input, input[1])
    ys <- c(ys, ys[1])
    pred <- predict(nmm, input, what="cdens")
    pred.scl <- pred*NA
    for (i in 1:nmm$G)
      pred.scl[,i] <- pred[,i]*nmm$parameters$pro[i]
    
    if (!is.null(x)) {
      hist(x, predUn=nmm$data, ...)
      lines(input, rowSums(pred.scl), lwd=3)
    } else {
      plot(nmm, what="density", lwd=3, ...)
    }
    polygon(xs, ys, col="gray")
    for (i in 1:nmm$G) {
      idx.sd <- ifelse(length(nmm$parameters$variance$sigmasq)==1, 1, i)
      # lines(input, pred[,i]*nmm$parameters$pro[i], lwd=2, col="darkgrey")
      lines(input, dnorm(input, 
                         mean=nmm$parameters$mean[i], 
                         sd=sqrt(nmm$parameters$variance$sigmasq[idx.sd]) )*
              nmm$parameters$pro[i], col="red", lwd=2 )
    }
    if (!is.null(empi))
      lines(input, dnorm(input, 
                         empi$mean, empi$sd)*nmm$parameters$pro[which.max(nmm$parameters$mean)], 
            lty=3, lwd=2)
    
    legend("topleft", legend=paste(names(tsts), signif(tsts, 3), sep=": "), cex=.8)
  }
  # diagnosticPlot(x, nmm, tsts, empi=empi)
  
  ### end of functions 
  ##############################################################################
  
  
  ### update trainOcc model x if modRow is given, else it is asumed
  if(!is.null(modRow)) {
    # modRow=modelPosition(x)$row
    x <- update(x, modRow=modRow)
  }
  hop <- holdOutPredictions(x)
  
  ### calculate nmm if it is not given
  if (is.null(nmm) & !is.null(un)) {
    nmm <- mclust(un)
  } else if (!is.null(nmm) & !is.null(un)) {
    stop("\"un\" or \"nmm\" must be given.")
  }
  
  if (aggregateHop | !is.list(hop$pos)) {
    hop <- aggregate(hop)
    hop$pos <- list(hop$pos) ### just that we can use the same loop
    hop$un <- list(hop$un)   ### as if aggregateHop=FALSE
  } 
  
  ### here a loop would begin in the bootstrap case
  pos <- hop$pos[[1]]
  if (predFromNumm)  {
    un <- nmm$data
  } else {
    un <- hop$un[[1]]
  }
  if (!is.null(predU)) {
    un <- predU
  }
  
  empi <- list(mean=mean(pos), 
               var=var(pos), 
               sd=sd(pos))
  theo <- getTheo(nmm)
  
  ### ------------------------------------
  ### overlaps
  olap <- getOverlap(nmm, theo)
  
  olap.pp <- getOverlap_PP(theo, empi)
  # plot_theoEmpi(theo, empi)
  
  ### ------------------------------------
  ### tests
  tsts <- c(myKruskal(pos, un), 
            myZ(pos, theo), 
            myChisq(pos, theo), 
            myKs(pos, theo), 
            olap=olap$value, 
            olap.pp=olap.pp$value, 
            pro.pos=nmm$parameters$pro[which.max(nmm$parameters$mean)])
  
  
  if (!is.null(fnamePlot)) {
    pdf(fnamePlot)
    diagnosticPlot(x, nmm, tsts, empi=empi)
    dev.off()
  } else {
    diagnosticPlot(x, nmm, tsts, empi=empi)
  }
  return(as.data.frame(matrix(tsts, nrow=1, dimnames=list(NULL, names(tsts)))))
}