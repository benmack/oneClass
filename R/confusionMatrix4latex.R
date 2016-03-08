#' confusionMatrix4latex
#'
#' Given an object of class confusionMatrix (from caret), a slim xtable is printed to be used in latex documents.
#'
#' ... 
#'
#' @param obj an object of class "confusionMatrix" \code{\link{confusionMatrix}}.
#' @param obj2 another object like obj.
#' @param obj3 another object like obj.
#' @param obj4 another object like obj.
#' @param ... arguments that can be passed to \code{\link{xtable}}, e.g. cap and .
#' @return one or more confusion matrices in latex table format.
#' @author Benjamin Mack
#' @seealso \code{\link{confusionMatrix}}, \code{\link{xtable}}
#' @examples
#' ### to do
#' @export
confusionMatrix4latex <- function(obj, obj2=NULL, obj3=NULL, obj4=NULL, ...) {
  
  require(xtable)
  
  kappa <- function (tbl) {
    cm <- list(abs=tbl)
    cm$abs <- rbind(cm$abs,colSums(cm$abs))
    cm$abs <- cbind(cm$abs,rowSums(cm$abs))
    
    kappa=( ( cm$abs[3,3]*sum(diag(cm$abs[1:2,1:2])) ) - 
              ( (sum(cm$abs[1,1:2])*sum(cm$abs[1:2,1]))+
                  (sum(cm$abs[2,1:2])*sum(cm$abs[1:2,2]))) ) / 
      ( (cm$abs[3,3]^2) - ((sum(cm$abs[1,1:2])*sum(cm$abs[1:2,1]))+
                             (sum(cm$abs[2,1:2])*sum(cm$abs[1:2,2]))) )
    return(kappa)
  }
  
  
  getTable <- function(obj) {
    
    xtbl <- matrix("", 5, 3)
    
    xtbl[1:2, 1:2] <- as.character(obj$table[1:2,1:2])
    
    # users accuracy pos
    xtbl[1,3] <- as.character(round(obj$byClass["Pos Pred Value"]*100, 2))
    # users accuracy neg
    xtbl[2,3] <- as.character(round(obj$byClass["Neg Pred Value"]*100, 2)) 
    # producers accuracy pos
    xtbl[3, 1] <- as.character(round(obj$byClass["Sensitivity"]*100, 2))
    # producers accuracy neg
    xtbl[3, 2] <- as.character(round(obj$byClass["Specificity"]*100, 2))
    
    xtbl[4,1] <- paste(round(obj$overall["Accuracy"]*100, 2))
    
    if (is.na(obj$overall["Kappa"])) {
      xtbl[5,1] <- paste(round(kappa(obj$table), 4))
    } else {
      xtbl[5,1] <- paste(round(obj$overall["Kappa"], 4))
    }
    rownames(xtbl) <- c("(+)", "(-)", "PA", "OA", "K")
    colnames(xtbl) <- c("(+)", "(-)", "UA")
    return(xtbl)
  }
  
  
  tbl <- getTable(obj)
  rm(obj)
  
  if(!is.null(obj2)) {
    tbl <- cbind(tbl, getTable(obj2))
  }
  if(!is.null(obj3)) {
    tbl <- cbind(tbl, getTable(obj3))
  }
  if(!is.null(obj4)) {
    tbl <- cbind(tbl, getTable(obj4))
  }
  xtbl <- xtable(tbl, digits=0, ...)
  
  
  algn <- "lccc"
  
  if(!is.null(obj2)) {
    algn <- "lccc|ccc"
  }
  if(!is.null(obj3)) {
    algn <- "lccc|ccc|ccc"
  }
  if(!is.null(obj4)) {
    algn <- "lccc|ccc|ccc|ccc"
  }
  
  align(xtbl) <- algn
  
  print(xtbl, hline.after=c(0, 3))
  
}
