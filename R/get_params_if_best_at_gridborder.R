#'@title Find the optimal parameters given it occurs at the grid borders. 
#'@description Grid-search is often used for parameter selection.
#'An optimization criteria is calculated  for pre-defined 
#'model parameters (e.g. sigma/gamma and C for the SVM).
#'When the optimum occurs at the limits of the parameters it is 
#'possible that extending the parameter range improves the 
#'model performance. This functions returns the values of the parameter
#'limits given the optimum occurs at the limits. 
#'@param x A data frame with one column for each parameter and 
#'the optimization criteria.
#'@param params The column name(s) of the parameters. 
#'Optional, if \code{NULL} it is assumed that the parameters are 
#'in the columns \code{1:(ncol(x)-1)} and the optimization criteria 
#'in the column \code{ncol(x)}.   
#'are parameter columns and the last one is the column holding 
#'the values of the optimization criteria.
#'@param optcrit The column name of the optimization criteria.
#'Optiona, if \code{NULL} the last column is used.
#'@return List with the optimums parameters given it occures at 
#'the limits of the parameters.
#'@examples {
#'x <- expand.grid(p1=1:3,
#'                      p2=2:5)
#'x$oc <- rnorm(nrow(x))
#'x[8, 3] <- max(x$oc) + 1 # no limit
#'get_params_if_best_at_gridborder(x, params=c("p1", "p2"), optcrit="oc")
#'x[4, 3] <- max(x$oc) + 1 # lower limit of p1
#'get_params_if_best_at_gridborder(x, params=c("p1", "p2"), optcrit="oc")
#'x[12, 3] <- max(x$oc) + 1 # upper limits of p1 and p2
#'get_params_if_best_at_gridborder(x, params=c("p1", "p2"), optcrit="oc")
#'}
#'@export
get_params_if_best_at_gridborder <- function(x, params=NULL, optcrit=NULL,
                                             ignore=NULL) {
  
  do_ignore <- function(ignore, param, direction) {
    if (is.null(ignore))
      return(FALSE)
    if (!param %in% names(ignore))
      return(FALSE)
    if (direction %in% ignore[[param]]) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  if (is.null(params) & is.null(optcrit)) {
    params <- colnames(x)[1:(ncol(x)-1)]
    optcrit <- colnames(x)[ncol(x)]
  } else if (!is.null(params) & !is.null(optcrit)) {
    # ...
  } else {
    stop("\"params\" and \"optcrit\" must BOTH be \"NULL\" or character/character vectors.")
  }
  
  u.params <- lapply(params, function(cname) unique(x[, cname])[[1]])
  names(u.params) <- params
  mx <- max(x[, optcrit])
  idx.mx <- which(x[, optcrit] == mx)
  
  params.mx <- vector(length(params), mode="list")
  names(params.mx) <- params
  params.mx.limit <- vector(length(params), mode="list")
  names(params.mx.limit) <- params
  for (param in params) {
    if (do_ignore(ignore, param=param, direction="lower") &
        do_ignore(ignore, param=param, direction="upper"))
      next
    params.mx[[param]] <- unique(x[idx.mx, param])
    if (min(params.mx[[param]]) == u.params[[param]][1] &
        !do_ignore(ignore, param=param, direction="lower")) {
      params.mx.limit[[param]] <- u.params[[param]][1]
      attr(params.mx.limit[[param]], "limit") <- "lower"
    }
    if (max(params.mx[[param]]) == u.params[[param]][length(u.params[[param]])] &
        !do_ignore(ignore, param=param, direction="upper")) {
      params.mx.limit[[param]] <- u.params[[param]][length(u.params[[param]])]
      attr(params.mx.limit[[param]], "limit") <- "upper"
    }
  }
  return(params.mx.limit)
}