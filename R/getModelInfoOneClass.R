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