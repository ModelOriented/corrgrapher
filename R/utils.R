#' Prepare corrgrapher for knitr and html
#' 
#' Wrap corrgrapher with tags necessary to preview it with knitr or in HTML.
#' 
#' @param cgr corrgrapher object.
#' 
#' @return 
#' A shiny.tag created with htmltools, consisting of 2 divs.
#' In one graph is inserted.
#' The other is a 'tabset' with plots of features/variables, shown on the side, with 1 feature at a time.
#' @importFrom stats runif
#' @import htmltools
#' @noRd
wrap_with_html_tag <- function(cgr) {
  if (!'corrgrapher' %in% class(cgr))
    stop("cgr must be of corrgrapher class")
  x <- {
    cgr_graph <- plot(cgr, width = '65%', height = '80%')
    cgr_graph <- visNetwork::visOptions(cgr_graph, nodesIdSelection = list(selected = 1))
    cgr_graph <- visNetwork::visEvents(cgr_graph, type = 'once', afterDrawing = 'addEventToSelect')
    cgr_graph <- visNetwork::visEvents(cgr_graph, 
                                       selectNode = 'showPlotOnSelect')
    base_id <- paste('cgr_content', as.character(round(runif(1, min = 1e5, max = 1e6-1))), sep = '_')
    
    nums <- which_variables_are_numeric(cgr$data)
    plots <- tagList(
      lapply({
        cgr$nodes$label
      }, function(name){
        if('pds' %in% names(cgr))
          if(nums[name])
            plt <- suppressWarnings(
              plot(cgr$pds[['numerical']],variables = name))
          else
            plt <- suppressWarnings(
              plot(cgr$pds[['categorical']],variables = name))
          else plt <- plot_distribution(cgr$data[[name]], name)
          tags$div(
            id = paste(base_id, name, sep = '_'),
            class = 'cgr_tabcontent',
            suppressMessages(htmltools::plotTag(plt,
                               alt = name,
                               width = 250,
                               height = 200,
                               attribs = list(class = 'cgr_image'),
                               suppressSize = "xy"))
          )
      })
    )
    
    tags$div(
      class = 'cgr_content',
      id = base_id,
      tags$div(
        class = 'cgr_graph',
        id = paste(base_id, 'graph', sep = '_'),
        cgr_graph
      ),
      tags$div(
        id = paste(base_id, 'tabpanel', sep = '_'),
        class = 'cgr_tabpanel',
        plots
      ),
      includeScript(system.file('d3js', 'graph-plot_communication.js', package = 'corrgrapher')),
      tags$script(paste0(
        'document.getElementById(\'',
        paste(base_id, cgr$nodes[cgr$nodes$id == 1,'label'], sep = '_'),
        '\').style.display = "block";'
      ))
    )
  }
  x <- attachDependencies(
    x,
    value = htmlDependency(
      src = system.file('d3js', package = 'corrgrapher'),
      version = '0.2',
      name = 'CorrGrapheRCSS',
      stylesheet = 'report.css'
    )
  )
  x
}

#' Plot distribuiton of a single variable
#' 
#' A helper function to visualize distribution of a single variable
#' 
#' @param x Variable to plot
#' @param label name of the variable
#' 
#' @return 
#' A ggplot2 plot - histogram for numerical (is.numeric) variables
#' and countplot for categorical (is.factor) variables
#' @noRd


plot_distribution <- function(x, label){
  # plot for x being a simple numeric or factor
  d <- data.frame(x = x)
  plt <- ggplot2::ggplot(data = d, ggplot2::aes(x = x)) +
    ggplot2::xlab(label) +
    ggplot2::labs(title = paste0('Distribution of ',label, ' variable'))
  if(is.numeric(x))
    plt <- plt + ggplot2::geom_histogram()
  else
    plt <- plt + ggplot2::geom_bar()
  plt
}

#' Extract info about column types
#' 
#' @param data data.frame or matrix
#' 
#' @return A boolean vector of ncol(data) length, answering the question, wheter ith variable is numeric 
#' @noRd

which_variables_are_numeric <- function(data) {
  if (is.matrix(data)) {
    apply(data[,, drop = FALSE], 2, is.numeric)
  } else {
    sapply(data[,, drop = FALSE], is.numeric)
  }
}
