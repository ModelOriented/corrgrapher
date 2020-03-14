#' @export
plot_corrgraph <- function(df, cutoff = 0.15){
  warning('Package in experimental state. Use this function ONLY with dfs which ALL columns are numeric 
          (i.e. calculating corelation with cor() makes sense).')
  corelations <- cor(df)
  coldict <- setNames(1:ncol(df), colnames(df))
  
  negative_corelation_handler <- function(x) x
  
  nodes <- data.frame(id = coldict,
                      label = names(coldict),
                      size = 20)
  corelations <- as.vector(cor(df))
  edges <- data.frame(corelations = corelations, 
                      from = rep(1:ncol(df), each = ncol(df)),
                      to = rep(1:ncol(df), times = ncol(df)),
                      length = negative_corelation_handler(1.1 - abs(corelations)) * 500,
                      # dashes = abs(corelations) < no_corelation_approx,
                      # opacity = ifelse(abs(corelations) < no_corelation_approx,
                      #                  0.3,
                      #                  1),
                      color = ifelse(abs(corelations) < cutoff,
                                     'grey',
                                     ifelse(corelations >= 0, 'blue', 'red')),
                      label = as.character(round(corelations, 2)),
                      width = abs(corelations) * 2,
                      smooth = FALSE)
  edges <- edges[edges$from < edges$to, ]
  net <- visNetwork::visNetwork(nodes, 
                    edges[,-which(colnames(edges)=='corelations')], height = 600, width = 1000)
  net
}
