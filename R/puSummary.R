################################################################################
#' puSummary
#'
#' ... .
#'
#' ... .
#'
#' @param data ...
#' @param lev ...
#' @param model ...
#' @return ...
#' @examples  \dontrun{
#' ### create a PU-data example of random numbers
#' P <- rnorm( 30, 2 )
#' U <- c( rnorm( 80 ), rnorm( 20, 2 ) )
#' d <- data.frame( obs = puFactor( rep( c( 1, 0 ), c( 30, 100 ) ), positive = 1 ), 
#'                  pos = c( P, U ) )
#' puSummary(d)
#' }
#' @rdname puSummary
#' @export 
puSummary <- function(data, lev = NULL, model = NULL) { # , metrics=c("puAuc", "puF", "Tpr", "puPpv")
  
  #if (nrow(data)>11)
  #  browser()
  
  #if (is.na(data[1, "pred"]))
  #  browser()
  
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  
  require(pROC)
  rocObject <- try(pROC:::roc(response=data$obs, predictor=data[, 'pos'], 
                             levels=c('pos', 'un')), silent = TRUE)
  puAuc <- ifelse (class(rocObject)[1] == "try-error", 0, rocObject$auc)
  
  tpr <- sum(data[,"pred"]=="pos" & data[,"obs"]=="pos")/sum(data[,"obs"]=="pos")
  puPpv <- (sum(data[,"pred"]=="pos")/length(data[,"pred"]))
  puF <- (tpr^2)/puPpv
  if (is.na(puF))
    puF[1] <- 0
  
  puF <- puF[[1]]
  
  negD01 <- oneClass:::.negD01(tpr=tpr, puPpv=puPpv)
  
  out <- c(puAuc=puAuc, puF=puF, negD01=negD01, Tpr=tpr, puPpv=puPpv) #, "SensPu", "SpecPu")
  
  return(out)
  
}