# funkcje sprawdzają parametry i zwracają przetworzone wartości
# rozdzielamy process() od check()

check_feature_importance <- function(feature_importance){
  if(!'feature_importance_explainer' %in% class(feature_importance) &
     !is.list(feature_importance) &
     !is.null(feature_importance))
    stop(paste0('`feature_importance`, if supported, must be a feature_importance_explainer or list, not', 
                class(feature_importance)[1]), 
         call. = FALSE)
}

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

check_partial_dependence <- function(partial_dependence, x){
  nums <- which_variables_are_numeric(x$data)
  cats <- !nums
  if(is.null(partial_dependence)) return()
  if(!is.list(partial_dependence)) stop(paste0('`partial_dependence`, if supported, must be a list, not', 
                                               class(feature_importance)[1]), 
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
          class(feature_importance)[1]
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
          class(feature_importance)[1]
        ),
        call. = FALSE
      )
  }
}

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