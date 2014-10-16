.onLoad <- function(...) {
  
  hints <- c(
    "For citation in published research please see \'citation(\'oneClass\')\'.\n"
    # "Use suppressPackageStartupMessages to eliminate package startup messages."
  )
  packageStartupMessage(hints)
  
}