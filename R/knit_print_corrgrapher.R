#' Knitr S3 method
#' 
#' This method allows \code{corrgrapher} objects to be displayed nicely in knitr/rmarkdown documents.
#' 
#' @param x An object of \code{corrgrapher} class. See \code{\link{corrgrapher}} function.
#' @param ... Other parameters, passed directly to \code{\link[htmltools]{knit_print.shiny.tag}}
#' @return 2 objects will be displayed: graph of correlations on the left and a plot on the right. 
#' If \code{x} was created from \code{explainer}, the plot will visualize partial dependency
#' of the currently selected variable.
#' In other case, the plot will visualize distribution of the variable.
#' 
#' @importFrom knitr knit_print
#' @method knit_print corrgrapher
#' @export
knit_print.corrgrapher <- function(x, ...){
  x <- wrap_with_html_tag(x)
  htmltools::knit_print.shiny.tag(x)
}