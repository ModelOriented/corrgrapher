library(shiny)
library(visNetwork)
library(CorrGrapheR)
library(ingredients)
library(gbm)
data('fifa_gbm_exp', package = 'CorrGrapheR')
data('fifa_feat', package = 'CorrGrapheR')
cgr <- create_corrgrapher(fifa_gbm_exp, feature_importance = fifa_feat)
ui <- fluidPage(

    titlePanel("Package prototype with FIFA dataset"),
    fluidRow(
        column(2,
               'Parameters for the graph <not working yet>',
               sliderInput('cutoff_input',
                           'choose cutoff parameter',
                           min = 0,
                           max = 1,
                           value = 0.4,
                           step = 0.05),
               sliderInput('feature_importance_cutoff_input',
                           'choose % of most important features to be shown',
                           min = 0,
                           max = 100,
                           value = 100
                           ),
               style = "background-color:#f0f0f4;"),
        column(7,
               'Graph',
               visNetworkOutput('graph', height = '600px')),
        column(3,
               'Partial dependency plot',
               plotOutput('pdp'),
               style = "background-color:#f0f0f4;"
        )
    )
)


server <- function(input, output) {
    output$graph <- renderVisNetwork({
            net <- plot(cgr)
            net <- visExport(net)
            visOptions(net, 
                       highlightNearest = TRUE,
                       nodesIdSelection = list(enabled = TRUE, selected = "1"))
        })
    selected_node <- reactiveVal('age')
    observeEvent(input[['graph_selected']],
                 if(!input[['graph_selected']] == "")
                     selected_node(cgr$nodes[cgr$nodes$id == input[['graph_selected']], 'label']))
    output$pdp <- renderPlot({
        plot(partial_dependence(fifa_gbm_exp, variables = selected_node()))
    })
}


shinyApp(ui = ui, server = server)
