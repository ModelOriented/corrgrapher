## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE)
data('fifa_cgr', package = 'CorrGrapheR')

## ----cars, message=FALSE------------------------------------------------------
library('CorrGrapheR')
library('magrittr')
df <- as.data.frame(datasets::Seatbelts)[,-8] # Drop the binary variable
cgr <- corrgrapher(df)

## ----cars_plot----------------------------------------------------------------
cgr

## ----titanic, cache=TRUE------------------------------------------------------
library(ranger)
library(ingredients)
library(DALEX)
data("titanic_imputed", package='DALEX')
tit_model <- ranger(survived ~ ., data = titanic_imputed, num.trees = 100)
tit_model_exp <- explain(tit_model,
                         data = titanic_imputed[,-8],
                         y = titanic_imputed[, 8],
                         verbose = FALSE)
tit_cgr <- corrgrapher(tit_model_exp)

## ----titanic_plot-------------------------------------------------------------
tit_cgr

## ----fifa_show, eval=FALSE----------------------------------------------------
#  library("gbm")
#  
#  data('fifa20', package = 'CorrGrapheR')
#  fifa20_selected <- fifa20[,c(4,5,7,8,11:13,17,26,45:78)]
#  
#  # Value is skewed. Will be much easier to model sqrt(Value).
#  
#  fifa20_selected$value_eur <- log10(fifa20_selected$value_eur)
#  fifa20_selected$team_position <- factor(fifa20_selected$team_position)
#  fifa20_selected <- na.omit(fifa20_selected)
#  fifa20_selected <- fifa20_selected[fifa20_selected$value_eur > 0,]
#  fifa20_selected <- fifa20_selected[!duplicated(fifa20_selected[,1]),]
#  rownames(fifa20_selected) <- fifa20_selected[,1]
#  fifa20_selected <- fifa20_selected[,-1]
#  
#  # create a gbm model
#  
#  set.seed(1313)
#  
#  # 4:5 are overall and potential, too strong predictors
#  fifa_gbm <- gbm(value_eur ~ . , data = fifa20_selected[,-(4:5)], n.trees = 250, interaction.depth = 3)
#  
#  # Create DALEX explainer
#  
#  fifa_gbm_exp <- DALEX::explain(fifa_gbm,
#                          data = fifa20_selected[, -6],
#                          y = 10^fifa20_selected$value_eur,
#                          predict_function = function(m,x)
#                            10^predict(m, x, n.trees = 250))
#  
#  fifa_feat <- ingredients::feature_importance(fifa_gbm_exp)
#  fifa_pd <- ingredients::partial_dependency(fifa_gbm_exp)
#  # Finally, create a corrgrapher object
#  fifa_cgr <- corrgrapher(fifa_gbm_exp, cutoff = 0.4,
#                          feature_importance = fifa_feat,
#                          partial_dependency = list(numerical = fifa_pd))

## ----fifa_plot----------------------------------------------------------------
fifa_cgr

## ----dragons_plot-------------------------------------------------------------
dragons_cgr

## ----html, eval=FALSE---------------------------------------------------------
#  ## NOT RUN
#  save_cgr_to_html(fifa_cgr)

