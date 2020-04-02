#' Knitr S3 methods
#' 
#' These methods allow \code{corrgrapher} objects to be displayed nicelu in knitr/rmarkdown documents.
#' 
#' @param x An object of \code{corrgrapher} class. See \code{\link{craete_corrgrapher}} function.
#' @param ... Other parameters, passed directly to \code{\link{htmltools::knit_print.shiny.tag}}
#' @return If \code{x} was created from \code{explainer}, 2 objects will be displayed: graph of correlations
#' on the left and partial dependency plot on the right.
#' In other case, just a graph will be displayed.
#' 
#' @export
knit_print.corrgrapher <- function(x, ...){
  x <- wrap_with_html_tag(x)
  x <- attachDependencies(x, value = htmlDependency(src = system.file('d3js', package = 'CorrGrapheR'),
                                               version = '0.2',
                                               name = 'CorrGrapheRCSS',
                                               stylesheet = 'report.css'))
  htmltools::knit_print.shiny.tag(x)
}