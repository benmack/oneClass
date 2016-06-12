#' Activate r-oneClass environment and calls jupyter notebook.
#'
#' @return NULL
#' @export
#'
#' @examples
jupyter_notebook <- function() {
  system("activate r-oneClass & jupyter notebook")
}