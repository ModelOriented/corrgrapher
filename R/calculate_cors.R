#' Calculate correlation coefficients
#' 
#' Calculate correlation coefficients between variables in a \code{data.frame}, \code{matrix} or \code{table} 
#' using 3 different functions for 3 different possible pairs of vairables:
#' \itemize{
#' \item{numeric - numeric}
#' \item{numeric - categorical}
#' \item{categorical - categorical}}
#' 
#' @param x object used to select method. See more below.
#' @param num_num_f A \code{function} used to determine correlation coefficient between a pair of numeric variables
#' @param num_cat_f A \code{function} used to determine correlation coefficient between a pair of numeric and categorical variable
#' @param cat_cat_f A \code{function} used to determine correlation coefficient between a pair of categorical variables
#' @param max_cor A number used to indicate absolute correlation (like 1 in \code{cor}). Must be supplied if any of \code{*_f} arguments is supplied.
#' 
#' @section X argument:
#' 
#' When \code{x} is a \code{data.frame}, all columns of \code{numeric} type are treated as numeric variables and all columns of \code{factor} type are treated as categorical variables. Columns of other types are ignored.
#' 
#' When \code{x} is a \code{matrix}, it is converted to \code{data.frame} using \code{\link{as.data.frame.matrix}}.
#' 
#' When \code{x} is a \code{explainer}, the tests are performed on its \code{data} element.
#' 
#' When \code{x} is a \code{table}, it is treated as contingency table. Its dimensions must be named, but none of them may be named \code{Frequency}.
#' 
#' 
#' @section Default functions:
#' 
#' By default, the function calculates p_value of statistical tests ( \code{\link[stats]{cor.test}} for 2 \code{numeric}, \code{\link[stats]{chisq.test}} for \code{factor} and \code{\link[stats]{kruskal.test}} for mixed).
#' 
#' Then, the correlation coefficients are calculated as \code{-log10(p_value)}. Any results above 100 are treated as absolute correlation and cut to 100. 
#' 
#' The results are then divided by 100 to fit inside [0,1].
#' 
#' If only \code{numeric} data was supplied, the function used is \code{\link[stats]{cor.test}}.
#' 
#' @section Custom functions:
#' 
#' Creating consistent measures for correlation coefficients, which are comparable for different kinds of variables, is a non-trivial task.
#' Therefore, if user wishes to use custom function for calculating correlation coefficients, he must provide \strong{all} necessary functions.
#' Using a custom function for one case and a default for the other is consciously not supported. 
#' Naturally, user may supply copies of default functions at his own responsibility.
#' 
#' Function \code{calculate_cors} chooses, which parameters of \code{*_f} are required based on data supported. 
#' For example, for a \code{matrix} with \code{numeric} data only \code{num_num_f} is required. 
#' On the other hand, for a \code{table} only \code{cat_cat_f} is required.
#' 
#' All \code{*_f} parameters must be functions, which accept 2 parameters (\code{numeric} or \code{factor} vectors respectively)
#' and return a single number from [0,max_num]. The \code{num_cat_f} must accept \code{numeric} argument as first and \code{factor} argument as second.
#' 
#' @return 
#' A symmetrical \code{matrix} A of size n x n, where n - amount of columns in \code{x} (or dimensions for \code{table}).
#' The value at A(i,j) is the correlation coefficient between ith and jth variable.
#' On the diagonal, values from \code{max_cor} are set.
#' 
#' @examples 
#' 
#' data(mtcars)
#' # Make sure, that categorical variables are factors
#' mtcars$vs <- factor(mtcars$vs, labels = c('V-shaped', 'straight'))
#' mtcars$am <- factor(mtcars$am, labels = c('automatic', 'manual'))
#' calculate_cors(mtcars)
#' 
#' # For a table:
#' data(HairEyeColor)
#' calculate_cors(HairEyeColor)
#' 
#' # Custom functions:
#' num_mtcars <- mtcars[,-which(colnames(mtcars) %in% c('vs', 'am'))]
#' my_f <- function(x,y) cor.test(x, y, method = 'spearman', exact=FALSE)$estimate
#' calculate_cors(num_mtcars, num_num_f = my_f, max_cor = 1)
#' 
#' @seealso  \code{\link[stats]{cor.test}}, \code{\link[stats]{chisq.test}}, \code{\link[stats]{kruskal.test}}
#' @importFrom stats setNames
#' @importFrom stats cor.test
#' @importFrom stats chisq.test
#' @importFrom stats kruskal.test
#' @importFrom stats xtabs
#' @importFrom utils combn
#' 
#' @rdname calculate_cors
#' @export

calculate_cors <- function(x,
                           num_num_f = NULL,
                           num_cat_f = NULL,
                           cat_cat_f = NULL,
                           max_cor = NULL){
  UseMethod('calculate_cors')
  
}

#' @rdname calculate_cors
#' @export

calculate_cors.explainer <- function(x,
                                  num_num_f = NULL,
                                  num_cat_f = NULL,
                                  cat_cat_f = NULL,
                                  max_cor = NULL) {
  x <- x$data
  NextMethod()
}

#' @rdname calculate_cors
#' @export

calculate_cors.matrix <- function(x,
                                  num_num_f = NULL,
                                  num_cat_f = NULL,
                                  cat_cat_f = NULL,
                                  max_cor = NULL) {
  x <- as.data.frame(x)
  NextMethod()
}

#' @rdname calculate_cors
#' @export

calculate_cors.table <- function(x,
                                 num_num_f = NULL,
                                 num_cat_f = NULL,
                                 cat_cat_f = NULL,
                                 max_cor = NULL) {
  if(!is.null(cat_cat_f)){
    if(!is.function(cat_cat_f))
      stop(paste0('If provided, cat_cat_f must be a function ,not',
                  class(cat_cat_f))
      )
    if(is.null(max_cor)) stop('max_cor must be provided')
  }
  else {
    cat_cat_f <- function(x) -log10(chisq.test(x)[['p.value']])
    max_cor <- 100
  }
  df <- as.data.frame(x, responseName = 'Frequency')
  pairs <- combn(names(dimnames(x)), 2)
  values <- apply(pairs, 2, function(v){
    df_cut <- df[,c(v, 'Frequency')]
    tab <- xtabs(formula = Frequency ~ ., data = df_cut)
    cat_cat_f(tab)
  })
  values <- pmin(values, max_cor) / max_cor
  values_to_matrix(values, pairs, names(dimnames(x)))
}

#' @rdname calculate_cors
#' @export

calculate_cors.default <- function(x,
                                      num_num_f = NULL,
                                      num_cat_f = NULL,
                                      cat_cat_f = NULL,
                                      max_cor = NULL) {
  # sprawdzenie poprawności i potrzebnych funkcji
  if(!is.data.frame(x)) stop(paste0('x must be a data.frame, not ', class(x)))
  nums <- which_variables_are_numeric(x)
  cats <- sapply(x[,,drop=FALSE], is.factor)
  if(sum(nums) + sum(cats) < ncol(x)) warning(
    paste0('Ignoring columns:',
           do.call(paste, append(as.list(colnames(x)[!nums & !cats]),
                                 list(sep = ', ')))
    )
  )
  
  # Określamy, które z funkcji są niezbędne
  necessary_fs <- list(num_num_f = sum(nums) > 1,
                        num_cat_f = sum(nums) >= 1 && sum(cats) >= 1,
                        cat_cat_f = sum(cats) > 1)
  if(all(c(is.null(num_num_f), is.null(num_cat_f), is.null(cat_cat_f)))){
    if(!necessary_fs$num_cat_f && !necessary_fs$cat_cat_f){
      num_num_f <- function(x, y) cor.test(x, y)[['estimate']]
      max_cor <- 1
    }
    else {
      num_num_f <- function(x, y) -log10(cor.test(x, y)[['p.value']])
      num_cat_f <- function(x, y) -log10(kruskal.test(x, y)[['p.value']])
      cat_cat_f <- function(x, y) suppressWarnings(-log10(chisq.test(x, y)[['p.value']]))
      max_cor <- 100
    }
  } else{
    if(is.null(max_cor)) stop('max_cor must be provided')
    for(f_type in names(necessary_fs)){
      if(!necessary_fs[[f_type]]) next
      f <- get(f_type)
      if(is.null(f)) stop(paste0(f_type, ' is necessary'))
      if(!is.function(f)) stop(paste0(f_type, ' must be a function, not ', class(f)))
    }
  }
  c_nums <- colnames(x)[nums]
  c_cats <- colnames(x)[cats]
  x <- x[,nums | cats]
  pairs <- combn(colnames(x), 2)
  
  determine_case <- function(v){
    if(length(setdiff(v, c_nums)) == 0) return(num_num_f)
    if(length(setdiff(v, c_cats)) == 0) return(cat_cat_f)
    num_cat_f
  }
  
  values <- apply(pairs, 2, function(v) determine_case(v)(x[[v[1]]],x[[v[2]]]))
  values <- pmin(values, max_cor) / max_cor
  values_to_matrix(values, pairs, colnames(x))
}

values_to_matrix <- function(values, pairs, cnames){
  df <- data.frame(t(pairs), values)
  names(df) <- c('Var1', 'Var2', 'values')
  df <- rbind(df,
              setNames(df[,c(2,1,3)], c('Var1', 'Var2', 'values')),
              data.frame(Var1 = cnames,
                         Var2 = cnames,
                         values = 1))
  out_data <- merge(expand.grid(cnames, cnames), df, sort = FALSE)
  out <- matrix(out_data$values, nrow = length(cnames))
  colnames(out) <- rownames(out) <- cnames
  out
}


