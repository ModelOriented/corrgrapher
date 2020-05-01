#' Check feature_importance argument
#' 
#' Test, whether feature_importance argument is valid, and raise errors in case.
#' 
#' @param feature_importance Object to check.
#' 
#' @noRd
check_feature_importance <- function(feature_importance){
  if(!'feature_importance_explainer' %in% class(feature_importance) &
     !is.list(feature_importance) &
     !is.null(feature_importance))
    stop(paste0('`feature_importance`, if supported, must be a feature_importance_explainer or list, not', 
                class(feature_importance)[1]), 
         call. = FALSE)
}

#' Process feature_importance argument
#' 
#' Extract or calculate importance of features, depending on argument
#' 
#' @param feature_importance Object to process.
#' 
#' @details 
#' 
#' If feature_importance is a feature_importance_explainer, it is simply passed further.
#' If it is NULL, it is calculated with default parameters.
#' If it's a list, it's assumed to be a list of arguments to pass to feature_importance().
#' 
#' @noRd
process_feature_importance <- function(feature_importance, x){
  # 1) obiekt klasy feature_importance_explainer
  if('feature_importance_explainer' %in% class(feature_importance))
    values <- feature_importance
  # 2) lista argumentów do feature_importance
  else if (is.list(feature_importance)){
    feature_importance$x <- x
    values <-
      do.call(ingredients::feature_importance,
              feature_importance)
  }
  else 
    values <- ingredients::feature_importance(x)
  values <- values[values$permutation == 0,c('variable', 'dropout_loss')]
  colnames(values) <- c('label', 'value')
  values
}

#' Check partial_dependency argument
#' 
#' Test, whether partial_dependency argument is valid, and raise errors in case.
#' 
#' @param partial_dependence Object to check.
#' @param x Explainer, based on which partial_dependence was created.
#' 
#' @noRd


check_partial_dependence <- function(partial_dependence, x){
  nums <- which_variables_are_numeric(x$data)
  cats <- !nums
  if(is.null(partial_dependence)) return()
  if(!is.list(partial_dependence)) stop(paste0('`partial_dependence`, if supported, must be a list, not', 
                                               class(partial_dependence)[1]), 
                                        call. = FALSE)
  if('aggregated_profiles_explainer' %in% class(partial_dependence)) stop('`partial_dependence_explainer` must be wrapped with list and named `numerical` or `categorical`')
  # partial_dependence jest listą
  # sprawdzamy, czy ma wymagane pola
  if(any(nums)) {
    if (is.null(partial_dependence[['numerical']]))
      stop('`partial_dependence` list must have a `numerical` element')
    if (!'aggregated_profiles_explainer' %in% class(partial_dependence[['numerical']]) &
        !is.list(partial_dependence[['numerical']]))
      stop(
        paste0(
          '`partial_dependence[[numerical]]`, if supported, must be a partial_dependence_explainer or list, not',
          class(partial_dependence)[1]
        ),
        call. = FALSE
      )
  }
  if(any(cats)){
    if (is.null(partial_dependence[['categorical']]))
      stop('`partial_dependence` list must have a categorical field')
    if (!'aggregated_profiles_explainer' %in% class(partial_dependence[['categorical']]) &
        !is.list(partial_dependence[['categorical']]))
      stop(
        paste0(
          '`partial_dependence[[categorical]]`, if supported, must be a partial_dependence_explainer or list, not',
          class(partial_dependence)[1]
        ),
        call. = FALSE
      )
  }
}

#' Process partial_dependence argument
#' 
#' Extract or calculate partial_dependences, depending on argument
#' 
#' @param partial_dependence Object to process.
#' @param x Explainer, based on which partial_dependence was created
#' 
#' @details 
#' partial dependence must be a list with numerical and categorical, depending on which are necessary for x.
#' If these elements are of partial_dependence_explainer class, they are simply passed further.
#' If it's a list, it's assumed to be a list of arguments to pass to partial_dependence().
#' 
#' @noRd

process_partial_dependence <- function(partial_dependence, x){
  # 1) czego potrzebujemy
  nums <- which_variables_are_numeric(x$data)
  cats <- !nums
  if(is.null(partial_dependence)){
    if(any(nums))
      partial_dependence$numerical <- ingredients::partial_dependence(x, variable_type = 'numerical')
    if(any(cats)) 
      partial_dependence$categorical <- ingredients::partial_dependence(x, variable_type = 'categorical')
    return(partial_dependence)
  }
  if(any(nums)) {
    if (!'aggregated_profiles_explainer' %in% class(partial_dependence[['numerical']])) {
      partial_dependence[['numerical']]$x <- x
      partial_dependence[['numerical']][['variable_type']] <- 'numerical'
      partial_dependence[['numerical']] <-
        do.call(ingredients::partial_dependence, partial_dependence[['numerical']])
    }
  }
  if(any(cats)) {
    if (!'aggregated_profiles_explainer' %in% class(partial_dependence[['categorical']])) {
      partial_dependence[['categorical']]$x <- x
      partial_dependence[['categorical']][['variable_type']] <- 'categorical'
      partial_dependence[['categorical']] <-
        do.call(ingredients::partial_dependence, partial_dependence[['categorical']])
    }
  }
  partial_dependence
}

#' Check cutoff argument
#' 
#' Test, whether cutoff argument is valid, and raise errors in case.
#' 
#' @param cutoff Object to check.
#' 
#' @noRd

check_cutoff <- function(cutoff){
  if(length(cutoff) > 1 || !is.numeric(cutoff)) stop('cutoff must be a single number')
  if(cutoff >= 1) warning('cutoff > 1. Interpreting as no cutoff')
  if(cutoff <= 0) warning('cutoff <= 0. Cutting off all edges')
}

#' Check values argument
#' 
#' Test, whether values argument is valid, and raise errors in case.
#' 
#' @param values Object to check.
#' @param x A data.frame passed to corrgraopher(), for which the values are supported.
#' 
#' @noRd


check_values <- function(values, x){
  if(!is.data.frame(values)) stop('if suported, values must be a data.frame')
  if(length(setdiff(c('label', 'value'), colnames(values))) > 0) stop('if suported, values must contain "label" and "value" columns')
  if(length(setdiff(colnames(x), values[['label']])) > 0) stop('if supported, values$label must contain all colnames(x)')
  if(!is.numeric(values$value)) stop('if supported, values$value must be numeric')
}