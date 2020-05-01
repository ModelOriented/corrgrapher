#' Generate and save HTML report
#' 
#' Create an interactive document in HTML based on \code{corrgrapher} object.
#' 
#' @param cgr An object of \code{corrgrapher} class. See \code{\link{corrgrapher}} function.
#' @param file File to write content to; passed directly to \code{\link[htmltools]{save_html}}.
#' @param overwrite If \code{file} exists, should it be overwritten?
#' @param ... Other parameters
#' @return  A file of \code{file} name will be generated with 2 elements: graph of correlations in the middle and a plot on the right. 
#' If \code{x} was created from \code{explainer}, the plot will visualize partial dependency
#' of the currently selected variable.
#' In other case, the plot will visualize distribution of the variable.
#' @import htmltools
#' @export
save_to_html <- function(cgr, file = 'report.html', overwrite = FALSE,...){
  if(!'corrgrapher' %in% class(cgr)) stop("cgr must be of corrgrapher class")
  if(file.exists(file)) {
    if(!overwrite) stop(paste(file, 'exists!'))
    file.remove(file)
  }
  content <- wrap_with_html_tag(cgr)
  doc <- tags$html(
    tags$head(
      #includeCSS(system.file('d3js', 'report.css', package = 'corrgrapher')),
      tags$title('CorrGrapheR report')
    ),
    tags$body(
      tags$div(
        class = 'cgr_container',
        content
      )
    )
  )
  htmltools::save_html(doc, file)
}