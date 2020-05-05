#' Create a \code{corrgrapher} object
#' 
#' This is the main function of \code{corrgrapher} package. It does necessary calculations and creates a \code{corrgrapher} object. 
#' Feel free to pass it into \code{plot}, include it in knitr report or generate a simple HTML. 
#' 
#' Data analysis (and creating ML models) involves many stages. For early exploration, 
#' it is useful to have a grip not only on individual series (AKA variables) available, 
#' but also on relations between them. Unfortunately, 
#' the task of understanding correlations between variables proves to be difficult. 
#' corrgrapher package aims to plot correlations between variables in form of a graph. 
#' Each node on it is associated with single variable. Variables correlated with each other 
#' (positively and negatively alike) shall be close, and weakly correlated  - far from each other.
#' 
#' @importFrom stats cor
#' @param x an object to be used to select the method, which must satisfy conditions:
#' \itemize{
#' \item{if \code{data.frame} (default), columns of \code{numeric} type must contain numerical variables and columns of \code{factor} class must contain categorical variables. Columns of other types will be ignored.}
#' \item{if \code{explainer}, methods \code{\link[ingredients]{feature_importance}} and \code{\link[ingredients]{partial_dependence}} must not return an error. 
#' See also arguments \code{feature_importance} and \code{partial_dependence}.}
#' \item{if \code{matrix}, it will be converted with \code{\link{as.data.frame}}.}
#' }
#' @param ... other arguments.
#' @param cutoff a number. Correlations below this are treated as \strong{no} correlation. Edges corresponding to them will \strong{not} be included in the graph.
#' @param values a \code{data.frame} with information about size of the nodes, containing columns \code{value} and \code{label} (consistent with colnames of \code{x}). Default set to equal for all nodes, or (for \code{explainer}) importance of variables.
#' @param cor_functions a named \code{list} of functions to pass to \code{\link{calculate_cors}}. Must contain necessary functions from \code{num_num_f}, \code{num_cat_f} or \code{cat_cat_f}. Must contain also \code{max_cor}
#' @param feature_importance Either: \itemize{
#' \item{an object of \code{feature importance_explainer} class, created by \code{\link[ingredients]{feature_importance}} function, or}
#' \item{a named \code{list} of parameters to pass to \code{\link[ingredients]{feature_importance}} function.}
#' }
#' @param partial_dependence a named \code{list} with 2 elements: \code{numerical} and \code{categorical}. Both of them should be either: \itemize{
#'  \item{an object of \code{aggregated_profile_explainer} class, created by \code{\link[ingredients]{partial_dependence}} function, or}
#'  \item{a named \code{list} of parameters to pass to \code{\link[ingredients]{partial_dependence}}}. 
#'  }
#'  If only one kind of data was used, use a list with 1 object.
#' @return A \code{corrgrapher} object. Essentially a \code{list}, consisting of following fields:
#' \itemize{
#' \item{\code{nodes} - a \code{data.frame} to pass as argument \code{nodes} to \code{\link{visNetwork}} function}
#' \item{\code{edges} - a \code{data.frame} to pass as argument \code{edges} to \code{\link{visNetwork}} function}
#' \item{\code{pds} (if x was of \code{explainer} class) - a \code{list} with 2 elements: \code{numerical} and \code{categorical}. Each of them contains an object of \code{aggregated_profiles_explainer} used to create partial dependency plots.}
#' \item{\code{data} - data used to create the object.}
#' }
#' @examples
#' # convert the category variable
#' df <- as.data.frame(datasets::Seatbelts)
#' df$law <- factor(df$law) 
#' cgr <- corrgrapher(df)
#' @seealso \code{\link{plot.corrgrapher}}, \code{\link{knit_print.corrgrapher}}, \code{\link{save_to_html}}
#' @rdname corrgrapher
#' @export

corrgrapher <- function(x, ...){
  UseMethod("corrgrapher")
}

#' @rdname corrgrapher
#' @export

corrgrapher.explainer <- function(x,
                                  cutoff = 0.2,
                                  # method = c('pearson', 'kendall', 'spearman'),
                                  values = NULL,
                                  cor_functions = list(),
                                  ...,
                                  feature_importance = NULL,
                                  partial_dependence = NULL) {
  # Check the parameters:
  # values and feature_importance:
  if (is.null(values)) {
    check_feature_importance(feature_importance)
    values <- process_feature_importance(feature_importance, x)
  } else{
    if (!is.null(feature_importance))
      warning('Supplied `values` and `feature_importance`. Ignoring `feature_importance`')
  }
  
  # partial_dependence:
  check_partial_dependence(partial_dependence, x)
  partial_dependence <- process_partial_dependence(partial_dependence, x)
  
  x <- x$data
  cgr <- NextMethod(cutoff = cutoff,
                    # method = method,
                    values = values,
                    cor_functions = cor_functions,
                    ...)
  cgr$pds <- partial_dependence
  cgr
}

#' @rdname corrgrapher
#' @export
corrgrapher.matrix <- function(x,
                               cutoff = 0.2,
                               values = NULL,
                               cor_functions = list(),
                               ...){
  x <- as.data.frame(x)
  NextMethod(cutoff = cutoff,
             values = values,
             cor_functions = cor_functions,
             ...)
}

#' @rdname corrgrapher
#' @export

corrgrapher.default <- function(x,
                                cutoff = 0.2,
                                # method = c('pearson', 'kendall', 'spearman'),
                                values = NULL,
                                cor_functions = list(),
                                ...) {
  # if(is.null(colorer)) colorer <- colorRampPalette(c('#9fe5bd80', '#77d1be80','#46bac280','#4590c480', '#371ea380'))
  if(!is.data.frame(x)) stop('x must be a data.frame')
  check_cutoff(cutoff)
  if(is.null(values)) values <- data.frame(value = rep(5 * sqrt(ncol(x)), ncol(x)),
                                       label = colnames(x))
  else {
    check_values(values, x)
    values <- values[,c('label', 'value')]
  }
  
  if(!is.null(cor_functions) && !is.list(cor_functions)) 
    stop(paste0('If supplied, cor_functions must be list, not', class(cor_functions)[1]))
  cor_functions$x <- x
  
  nodes <- data.frame(id = 1:ncol(x),
                      label = colnames(x),
                      title = colnames(x),
                      color = '#ae2c87',
                      scaling = list(label = list(min = 10, max = 15)),
                      stringsAsFactors = FALSE)
  
  nodes <- merge(nodes, values, 
                 by.x = 'label',
                 by.y = 'label')
  corelations <- as.vector(do.call(calculate_cors,
                                   cor_functions))
  
  edges <- data.frame(corelations = corelations, 
                      from = rep(1:ncol(x), each = ncol(x)),
                      to = rep(1:ncol(x), times = ncol(x)),
                      value = abs(corelations) * 2,
                      label = as.character(round(corelations, 2)),
                      length = (1.1 - abs(corelations)) * 100 * sqrt(ncol(x)),
                      hidden = abs(corelations) < cutoff,
                      color = ifelse(corelations >= 0, '#4378bf80', '#f05a7180'),
                      font = list(color = '#34343480',
                                  strokeWidth = 0),
                      smooth = FALSE,
                      scaling = list(min = 0.05,
                                     max = 5,
                                     label = list(min = 5, max = 8, maxVisible = 8)))
  edges <- edges[edges$from < edges$to,]
  edges$corelations <- NULL
  structure(list(nodes = nodes, 
                 edges = edges,
                 data = x),
            class = "corrgrapher")
}