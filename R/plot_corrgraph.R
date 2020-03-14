 library(visNetwork)
 library(dplyr)
 library(tidyr)
#' @export
plot_corrgraph <- function(df){
  warning('Package in experimental state. Use this function ONLY with dfs which ALL columns are numeric 
          (i.e. calculating corelation with cor() makes sense).')
  corelations <- cor(df)
  coldict <- setNames(1:ncol(df), colnames(df))
  no_corelation_approx <- 0.15
  negative_corelation_handler <- function(x) x
  
  nodes <- data.frame(id = coldict,
                      label = names(coldict))
  edges <- cor(df) %>% as.data.frame() %>%
    pivot_longer(cols = 1:ncol(df),
                 names_to = "to",
                 values_to = "strength") %>%
    mutate(from = rep(colnames(df), each = ncol(df))) %>%
    filter(from != to) %>%
    mutate(length = negative_corelation_handler(1.1 - strength) * 500,
           hidden = abs(strength) < no_corelation_approx,
           physics = !hidden,
           color = if_else(strength >= 0, 'blue', 'red'),
           label = as.character(round(strength, 2)),
           from = coldict[from],
           width = abs(strength) * 2 + 2,
           to = coldict[to]) %>%
    filter(from >= to)
  net <- visNetwork(nodes, select(edges, -strength), height = 600, width = 1000) %>%
    visNodes(size = 20) %>% 
    visEdges(smooth = FALSE) # %>%
  net
}