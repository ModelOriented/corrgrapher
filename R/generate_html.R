#' Generate HTML report
#' 
#' Create an interactive document in HTML based on \code{corrgrapher} object.
#' 
#' @param cgr An object of \code{corrgrapher} class. See \code{\link{craete_corrgrapher}} function.
#' @param file File to write content to; passed directly to \code{\link{htmltools::save_html}}.
#' @param rewrite If \code{file} exists, should it be overwritten?
#' @return  A file of \code{file} name will be generated with either 1 or 2 elements: 
#' \itemize{
#' \item{for \code{explainer}, a graph and partial dependency plot on the side.}
#' \item{for \code{data.frame}, just a graph.}
#' }
#' @import htmltools
#' @export
generate_html <- function(cgr, file = 'report.html', rewrite = FALSE,...){
  if(!'corrgrapher' %in% class(cgr)) stop("cgr must be of corrgrapher class")
  if(file.exists(file)) {
    if(!rewrite) stop(paste(file, 'exists!'))
    file.remove(file)
  }
  content <- plot_in_knitr(cgr)
  doc <- tags$html(
    tags$head(
      tags$title('CorrGrapheR report'),
    ),
    tags$body(
      content
    )
  )
  # doc
  save_html(doc, file)
}