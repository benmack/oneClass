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
puSummary <- function(data, lev = NULL, model = NULL, 
                            metrics=c('aucPu', 'fPu', 'rpr')) {
  
  
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  
  require(pROC)
  rocObject <- try(pROC::roc(response=data$obs, predictor=data[, 'pos'], 
                             levels=c('pos', 'un')), silent = TRUE)
  rocAUC <- ifelse (class(rocObject)[1] == "try-error", 0, rocObject$auc)
  out1 <- c(rocAUC #, sensitivity(data[, "pred"], data[, "obs"], 'pos'), 
            #specificity(data[, "pred"], data[, "obs"], 'un') 
  )
  
  recall <- sum(data$pred=="pos" & data$obs=="pos")/sum(data$obs=="pos")
  precision.pu <- (sum(data$pred=="pos")/length(data$pred))
  out <- data.frame(fPu=(recall^2)/precision.pu)
  if (is.na(out))
    out[1] <- 0
  out2 <- c(fPu=out[[1]], tpr=recall, ppvPu=precision.pu)
  
  out <- c(out1, out2)
  
  names(out) <- c("puAuc", "puF", "Tpr", "puPpv") #, "SensPu", "SpecPu")
  
  return(out)
  
}