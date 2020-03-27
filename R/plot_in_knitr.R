#' Plot in knitr
#' 
#' Display \code{corrgrapher} object in knitr document.
#' 
#' @param cgr An object of \code{corrgrapher} class. See \code{\link{craete_corrgrapher}} function.

#' @return  A file of \code{file} name will be generated with either 1 or 2 elements: 
#' \itemize{
#' \item{for \code{explainer}, a graph and partial dependency plot on the side.}
#' \item{for \code{data.frame}, just a graph.}
#' }
#' @import htmltools
#' @export
plot_in_knitr <- function(cgr, ...){
  if(!'corrgrapher' %in% class(cgr)) stop("cgr must be of corrgrapher class")
  if('pds' %in% names(cgr))
    content <- create_tabset(cgr)
  else
    content <- plot(cgr)
  content
}

create_tabset <- function(cgr){
  cgr_graph <- plot(cgr)
  # cgr_graph <- visEvents(cgr_graph, selectNode = "function(properties) {
  #     alert('selected nodes ' + this.body.data.nodes.get(properties.nodes[0]).id);}")
  # 
  css_tabcontent <- css(display='none')
  css_tab <- css(background.color = '#f1f1f1')
  base_id <- paste('cgr_content', as.character(round(runif(1, min = 1e5, max = 1e6-1))), sep = '_')
  plots <- tagList(
    lapply(names(cgr$pds), function(name){
      tags$div(
        id = paste(base_id, name, sep = '_'),
        class = 'cgr_tabcontent',
        plotly::ggplotly(plot(cgr$pds[[name]]))
      )
    })
  )
  
  create_option_tags <- function(name){
    tagList(
      lapply(name, function(name) HTML(paste0('<option value=',name,'>',name,'</option>')))
    )
  }
  
  tags$div(
    style = css(display = 'flex', width='100%'),
    id = base_id,
    includeScript(system.file('d3js', 'my_script.js', package = 'CorrGrapheR')),
    tags$div(
      style = css(flex = '60%'),
      id = paste(base_id, 'graph', sep = '_'),
      cgr_graph),
    tags$div(
      class = 'cgr_tabpanel',
      style = css(flex = '40%'),
      plots
    )
  )
}



