#' Create a CorrGrapheR object
#' 
#' Create a CorrGrapheR object before passing it to \code{plot()}. 
#' @importFrom stats cor
#' @param df a \code{data.frame}, in which for all \code{numeric} columns calling \code{\link{cor}} makes sense.
#' @param cutoff a number. Corelations below this are treated as \strong{no} corelation. Edges corresponding to them will \strong{not} be included in the graph.
#' @param method passed directly to \code{\link{cor}} function. 
#' @param ... other parameters.
#' 
#' @return A \code{corrgrapher} object.
#' @examples
#' df <- as.data.frame(datasets::Seatbelts)[,1:7] # drop the binary target variable
#' cgr <- create_corrgrapher(df)
#' @seealso \code{\link{plot.corrgrapher}}
#' @rdname create_corrgrapher
#' @export
create_corrgrapher <- function(df, ...){
  UseMethod("create_corrgrapher", df)
}

#' @rdname create_corrgrapher
#' @export

create_corrgrapher.default <- function(df, 
                                       cutoff = 0.2, 
                                       method = c('pearson', 'kendall', 'spearman'),
                                       ...){
  nums <- unlist(lapply(df, is.numeric))
  df <- df[,nums]
  corelations <- stats::cor(df, method = method)
  
  negative_corelation_handler <- function(x) x
  
  nodes <- data.frame(id = 1:ncol(df),
                      label = colnames(df),
                      title = colnames(df),
                      size = 5 * sqrt(ncol(df)))
  corelations <- as.vector(cor(df))
  edges <- data.frame(corelations = corelations, 
                      from = rep(1:ncol(df), each = ncol(df)),
                      to = rep(1:ncol(df), times = ncol(df)),
                      length = negative_corelation_handler(1.1 - abs(corelations)) * 50 * ncol(df),
                      hidden = abs(corelations) < cutoff,
                      # dashes = abs(corelations) < no_corelation_approx,
                      # opacity = ifelse(abs(corelations) < no_corelation_approx,
                      #                  0.3,
                      #                  1),
                      color = ifelse(corelations >= 0, 'blue', 'red'),
                      label = as.character(round(corelations, 2)),
                      width = abs(corelations) * 2,
                      smooth = FALSE,
                      scaling = list(label = TRUE))
  edges <- edges[edges$from < edges$to,]
  edges$corelations <- NULL
  structure(list(nodes = nodes, 
                 edges = edges),
            class = "corrgrapher")
}