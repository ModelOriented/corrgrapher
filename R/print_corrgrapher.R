#' Print S3 method
#' 
#' This method allows \code{corrgrapher} objects to be displayed nicely in RStudio viewer.
#' 
#' @param x An object of \code{corrgrapher} class. See \code{\link{corrgrapher}} function.
#' @param ... Other parameters, passed directly to \code{save_to_html} function.
#' If \code{x} was created from \code{explainer}, the plot will visualize partial dependency
#' of the currently selected variable.
#' In other case, the plot will visualize distribution of the variable.
#' 
#' @export
print.corrgrapher <- function(x, ...){
  tmpf <- tempfile(fileext = ".html")
  save_to_html(x, file = tmpf, overwrite = TRUE)
  viewer <- getOption("viewer")
  viewer(tmpf)
}