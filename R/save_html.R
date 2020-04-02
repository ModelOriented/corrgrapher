#' Generate & save HTML report
#' 
#' Create an interactive document in HTML based on \code{corrgrapher} object.
#' 
#' @param cgr An object of \code{corrgrapher} class. See \code{\link{corrgrapher}} function.
#' @param file File to write content to; passed directly to \code{\link[htmltools]{save_html}}.
#' @param overwrite If \code{file} exists, should it be overwritten?
#' @param ... Other parameters
#' @return  A file of \code{file} name will be generated with either 1 or 2 elements: 
#' \itemize{
#' \item{for \code{explainer}, a graph and partial dependency plot on the side.}
#' \item{for \code{data.frame}, just a graph.}
#' }
#' @import htmltools
#' @export
save_cgr_to_html <- function(cgr, file = 'report.html', overwrite = FALSE,...){
  if(!'corrgrapher' %in% class(cgr)) stop("cgr must be of corrgrapher class")
  if(file.exists(file)) {
    if(!overwrite) stop(paste(file, 'exists!'))
    file.remove(file)
  }
  content <- wrap_with_html_tag(cgr)
  doc <- tags$html(
    tags$head(
      includeCSS(system.file('d3js', 'report.css', package = 'CorrGrapheR')),
      tags$title('CorrGrapheR report'),
    ),
    tags$body(
      content
    )
  )
  htmltools::save_html(doc, file)
}