# Utils for creating HTML and knitr output
wrap_with_html_tag <- function(cgr, ...) {
  if (!'corrgrapher' %in% class(cgr))
    stop("cgr must be of corrgrapher class")
  x <- create_tabset(cgr)
  x <- attachDependencies(
    x,
    value = htmlDependency(
      src = system.file('d3js', package = 'CorrGrapheR'),
      version = '0.2',
      name = 'CorrGrapheRCSS',
      stylesheet = 'report.css'
    )
  )
  x
}

create_tabset <- function(cgr){
  cgr_graph <- plot(cgr, width = '100%',height = '100%')
  cgr_graph <- visNetwork::visOptions(cgr_graph, nodesIdSelection = list(selected = 1))
  cgr_graph <- visNetwork::visEvents(cgr_graph, type = 'once', afterDrawing = 'addEventToSelect')
  cgr_graph <- visNetwork::visEvents(cgr_graph, 
                                     selectNode = 'showPlotOnSelect')
  base_id <- paste('cgr_content', as.character(round(runif(1, min = 1e5, max = 1e6-1))), sep = '_')
  
  plots <- tagList(
    lapply({
      fact <- cgr$nodes$label
      chr <- attr(fact, 'levels')[as.integer(fact)]
      chr
      }, function(name){
        if('pds' %in% names(cgr))
          plt <- suppressWarnings(
            ingredients:::plot.aggregated_profiles_explainer(cgr$pds,
                                                              variables = name))
        else plt <- plot_distribution(cgr$data[[name]], name)
        tags$div(
          id = paste(base_id, name, sep = '_'),
          class = 'cgr_tabcontent',
          insert_image(
            plt,
            container_id = paste(base_id, name, sep = '_'),
            dir = dir
          )
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
    includeScript(system.file('d3js', 'graph-plot_communication.js', package = 'CorrGrapheR')),
    tags$script(paste0(
      'document.getElementById(\'',
      paste(base_id, cgr$nodes[cgr$nodes$id == 1,'label'], sep = '_'),
      '\').style.display = "block";'
    ))
  )
}

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

insert_image <- function(plt, container_id, tf = NULL, dir = tempdir()){
  # plt - obiekt, który da się zapisać do .png za pomocą png()
  if(is.null(tf)){
    tf <- tempfile(fileext = '.png')
    file.create(tf)
  }
  else{
    if(file.exists(tf)) stop(paste0(tf, ' exists'))
    file.create(tf)
  }
  suppressMessages(ggplot2::ggsave(tf, plt,
                                   width = 125,
                                   height = 125,
                                   units = 'mm'))
  txt <- RCurl::base64Encode(readBin(tf, "raw", file.info(tf)[1, "size"]), "txt")
  file.remove(tf)
  encoded_image_src <- sprintf('data:image/png;base64,%s', txt)
  tags$script(paste0("var img = document.createElement(\"img\");",
                     "img.classList.add(\"cgr_image\");",
                     "img.src = \"",
                     encoded_image_src,
                     "\";",
                     "var src = document.getElementById(\"",
                     container_id,
                     "\");",
                     "src.appendChild(img);"))
}