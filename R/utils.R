# Utils for creating HTML and knitr output
wrap_with_html_tag <- function(x, encode = TRUE, dir = tempdir(), ...) {
  if (!'corrgrapher' %in% class(x))
    stop("x must be of corrgrapher class")
  if ('pds' %in% names(x)) {
    x <- create_tabset(x, encode, dir)
    x <-
      attachDependencies(
        x,
        value = htmlDependency(
          src = system.file('CSS', package = 'CorrGrapheR'),
          version = '0.2',
          name = 'CorrGrapheRCSS',
          stylesheet = 'report.css'
        )
      )
  }
  else
    x <- plot(x)
  x
}

create_tabset <- function(x, encode = TRUE, dir = tempdir()){
  x_graph <- plot(x, width = '100%',height = '100%')
  x_graph <- visNetwork::visOptions(x_graph, nodesIdSelection = list(selected = 1))
  x_graph <- visNetwork::visEvents(x_graph, type = 'once', afterDrawing = 'addEventToSelect')
  x_graph <- visNetwork::visEvents(x_graph, 
                                     selectNode = 'showPlotOnSelect')
  base_id <- paste('cgr_content', as.character(round(runif(1, min = 1e5, max = 1e6-1))), sep = '_')
  plots <- tagList(
    lapply({
      fact <- x$nodes$label
      chr <- attr(fact, 'levels')[as.integer(fact)]
      chr
      }, function(name){
      tags$div(
        id = paste(base_id, name, sep = '_'),
        class = 'cgr_tabcontent',
        insert_image(
          suppressWarnings(ingredients:::plot.aggregated_profiles_explainer(x$pds,
                          variables = name)),
          container_id = paste(base_id, name, sep = '_'),
          encode = encode,
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
      x_graph
      ),
    tags$div(
      id = paste(base_id, 'tabpanel', sep = '_'),
      class = 'cgr_tabpanel',
      plots
    ),
    includeScript(system.file('d3js', 'graph-plot_communication.js', package = 'CorrGrapheR')),
    tags$script(paste0(
      'document.getElementById(\'',
      paste(base_id, x$nodes[x$nodes$id == 1,'label'], sep = '_'),
      '\').style.display = "block";'
    ))
  )
}

insert_image <- function(plt, container_id, tf = NULL, encode = TRUE, dir = tempdir()){
  # plt - obiekt, który da się zapisać do .png za pomocą png()
  if(is.null(tf)){
    tf <- tempfile(fileext = '.png', tmpdir = dir)
    file.create(tf)
  }
  else{
    if(file.exists(tf)) stop(paste0(tf, ' exists'))
    file.create(tf)
  }
  suppressMessages(ggplot2::ggsave(tf, plt,
                                   width = 75,
                                   height = 75,
                                   units = 'mm'))
  if(encode){
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
  else{
    tags$img(src = tf,
             class = 'cgr_image'
    )
  }
}