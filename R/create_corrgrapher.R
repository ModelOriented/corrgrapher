#' Create a CorrGrapheR object
#' 
#' Create a CorrGrapheR object before passing it to \code{plot()}. 
#' @importFrom stats cor
#' @param x a \code{data.frame}, in which for all \code{numeric} columns calling \code{\link{cor}} makes sense.
#' @param cutoff a number. Corelations below this are treated as \strong{no} corelation. Edges corresponding to them will \strong{not} be included in the graph.
#' @param method passed directly to \code{\link{cor}} function. 
#' @param feature_importance (Optional) an object of \code{feature importance_explainer} class, created by \code{\link{ingredients::feature_importance}} function. If not supported, calculated inside function.
#' @param values (Optional) a \code{data.frame} with information abour size of the nodes, containing columns \code{value} and \code{label} (consistent with colnames of \code{x}). Deafult set to equal for all nodes, or (for \code{explainer}) importance of variables.
#' @param ... other parameters.
#' 
#' @return A \code{corrgrapher} object.
#' @examples
#' df <- as.data.frame(datasets::Seatbelts)[,1:7] # drop the binary target variable
#' cgr <- create_corrgrapher(df)
#' @seealso \code{\link{plot.corrgrapher}}
#' @rdname create_corrgrapher
#' @export

create_corrgrapher <- function(x, ...){
  UseMethod("create_corrgrapher")
}

#' @rdname create_corrgrapher
#' @export

create_corrgrapher.explainer <- function(x,
                                         cutoff = 0.2,
                                         feature_importance = NULL,
                                         method = c('pearson', 'kendall', 'spearman')){
  if(is.null(feature_importance)){
    x_feat <- ingredients::feature_importance(x)
  } else {
    if(! 'feature_importance_explainer' %in% class(feature_importance)) stop('feature_importance must be of feature_importance_explainer class')
    x_feat <- feature_importance
  }
  x_feat <- x_feat[x_feat$permutation == 0, ]
  names(x_feat)[names(x_feat) %in% c('variable', 'dropout_loss', 'label')] <- c('label', 'value', 'model_label')
  cgr <- create_corrgrapher(x$data, 
                     cutoff = cutoff,
                     method = method,
                     values = x_feat)
  numerical_pds <- ingredients::partial_dependence(x, variable_type = 'numerical')
  numerical_pds_list <- split(numerical_pds, numerical_pds[['_vname_']], drop = TRUE)
  # categorical_pds <- ingredients::partial_dependence(x, variable_type = 'categorical')
  # categorical_pds_list <- split(categorical_pds, categorical_pds[['_vname_']], drop = TRUE)
  # cgr$pds <- append(numerical_pds_list, categorical_pds_list)
  cgr$pds <- numerical_pds_list
  cgr
}

#' @rdname create_corrgrapher
#' @export

create_corrgrapher.default <- function(x, 
                                       cutoff = 0.2, 
                                       method = c('pearson', 'kendall', 'spearman'),
                                       values = NULL,
                                       ...){
  # if(is.null(colorer)) colorer <- colorRampPalette(c('#9fe5bd80', '#77d1be80','#46bac280','#4590c480', '#371ea380'))
  if(!is.data.frame(x)) stop('x must be a data.frame')
  if(length(cutoff) > 1 || !is.numeric(cutoff)) stop('cutoff must be a single number')
  if(cutoff >= 1) warning('cutoff > 1. Interpreting as no cutoff')
  if(cutoff <= 0) warning('cutoff <= 0. Cutting off all edges')
  
  if(is.null(values)) values <- data.frame(value = rep(5 * sqrt(ncol(x)), ncol(x)),
                                       label = colnames(x))
  else {
    if(!is.data.frame(values)) stop('if suported, values must be a data.frame')
    if(length(setdiff(c('label', 'value'), colnames(values))) > 0) stop('if suported, values must contain "label" and "value" columns')
    if(length(setdiff(colnames(x), values[['label']])) > 0) stop('if supported, values$label must contain all colnames(x)')
    if(!is.numeric(values$value)) stop('if supported, values$value must be numeric')
    values <- values[,c('label', 'value')]
  }
  # else values$value <- normalize(values$value, 
  #                         out_mean = 5 * sqrt(ncol(x)),
  #                         out_sd = sqrt(ncol(x)))
  # 
  nums <- unlist(lapply(x, is.numeric))
  x <- x[,nums]
  
  nodes <- data.frame(id = 1:ncol(x),
                      label = colnames(x),
                      title = colnames(x),
                      color = '#ae2c87',
                      scaling = list(label = list(min = 10, max = 15)))
  nodes <- merge(nodes, values, 
                 by.x = 'label',
                 by.y = 'label')
  corelations <- as.vector(stats::cor(x, method = method))
  
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
                 edges = edges),
            class = "corrgrapher")
}