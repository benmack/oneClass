#' Tools for Models Available in \code{oneClass}
#' 
#' getModelInfoOneClass is adopted from \code{\link{getModelInfo}} of the 
#' \code{caret} package but returns models available in the package \code{oneClass}.
#' 
#' @param model a character string of a defined one-class classification method.
#' So far: ocsvm, biasedsvm, maxent.
#' @param regex a logical: should a regular expressions be used? If FALSE, a simple match is conducted against the whole name of the model.
#' @param ... options to pass to grepl
#' @return a list containing one or more lists of the standard model information
#' @seealso \code{\link{getModelInfo}}
#' @examples
#' getModelInfoOneClass("biasedsvm")
#' @export
getModelInfoOneClass <- function (model = NULL, regex = TRUE, ...) 
{
  load(system.file("models", "models.RData", package = "oneClass"))
  if (!is.null(model)) {
    keepers <- if (regex) 
      grepl(model, names(models), ...)
    else which(model == names(models))[1]
    models <- models[keepers]
  }
  models
}