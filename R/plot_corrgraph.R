#' @title
#' Visualize correlations in a corrgrapher object
#' 
#' @description 
#' Visualize correlations between variables, using previously created \code{corrgrapher} object.
#' 
#' @param x a \code{corrgrapher} object. See \code{\link{corrgrapher}}.
#' @param ... other parameters, passed directly to \code{\link{visNetwork}} function (such as \code{main}, \code{submain}, \code{width}, \code{height} etc.)
#' @return A \code{\link{visNetwork}} object; graph. On this graph, the edges are treated as springs. 
#'   The variables correlated \strong{strongly} (positively or negatively) are \strong{close} to each other, 
#'   and those not (or weakly) correlated - \strong{far} from each other.
#' @examples
#' df <- as.data.frame(datasets::Seatbelts)[,1:7] # drop the binary target variable
#' cgr <- corrgrapher(df)
#' plot(cgr)
#' @seealso \code{\link{corrgrapher}}
#' @method plot corrgrapher
#' @export

plot.corrgrapher <- function(x, ...){
  net <- visNetwork::visNetwork(nodes = x$nodes, 
                                edges = x$edges,
                                ...)
  net <- visNetwork::visOptions(net, highlightNearest = TRUE)
  net
}

#'
#' export
#' rdname plot.corrgraph
# plot_corrgraph <- function(df, cutoff = 0.2, method = c('pearson', 'kendall', 'spearman'), ...){
#   # warning('Package in experimental state. Use this function ONLY with dfs which ALL columns are numeric
#   #         (i.e. calculating corelation with cor() makes sense).')
#   plot(create_corrgrapher(df, cutoff, method),...)
# }
