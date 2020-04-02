# Utils for creating HTML and knitr output
wrap_with_html_tag <- function(cgr, ...){
  if(!'corrgrapher' %in% class(cgr)) stop("cgr must be of corrgrapher class")
  if('pds' %in% names(cgr))
    content <- create_tabset(cgr)
  else
    content <- plot(cgr)
  content
}

create_tabset <- function(cgr){
  cgr_graph <- plot(cgr)
  cgr_graph <- visNetwork::visOptions(cgr_graph, nodesIdSelection = list(selected = 1))
  cgr_graph <- visNetwork::visEvents(cgr_graph, type = 'once', afterDrawing = 'addEventToSelect')
  cgr_graph <- visNetwork::visEvents(cgr_graph, 
                                     selectNode = 'showPlotOnSelect')
  # css_tabcontent <- css(display='none')
  # css_tabpanel <- css(posistion = 'absolute',
  #                     top = '0px',
  #                     right = '0px',
  #                     border = '2px solid #160e3b',
  #                     z_index = 2)
  base_id <- paste('cgr_content', as.character(round(runif(1, min = 1e5, max = 1e6-1))), sep = '_')
  plots <- tagList(
    lapply({
      fact <- cgr$nodes$label
      chr <- attr(fact, 'levels')[as.integer(fact)]
      chr
      }, function(name){
      tags$div(
        id = paste(base_id, name, sep = '_'),
        class = 'cgr_tabcontent',
        # style = css_tabcontent,
        suppressWarnings(plotly::ggplotly(ingredients:::plot.aggregated_profiles_explainer(cgr$pds[[name]])))
      )
    })
  )
  
  tags$div(
    # style = css(width='100%', position = 'relative'),
    class = 'cgr_content',
    id = base_id,
    tags$div(
      # style = css(width = '100%', z_index = 1, border = '2px solid yellow'),
      class = 'cgr_graph',
      id = paste(base_id, 'graph', sep = '_'),
      cgr_graph
      #tags$p('I am the main game')
      ),
    tags$div(
      id = paste(base_id, 'tabpanel', sep = '_'),
      class = 'cgr_tabpanel',
      #style = css_tabpanel,
      plots
      #tags$p('I should be in corner')
    ),
    includeScript(system.file('d3js', 'graph-plot_communication.js', package = 'CorrGrapheR')),
    tags$script(paste0(
      'document.getElementById(\'',
      paste(base_id, cgr$nodes[cgr$nodes$id == 1,'label'], sep = '_'),
      '\').style.display = "block";'
    ))
  )
}

