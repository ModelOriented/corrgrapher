## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
data('fifa_cgr', package = 'CorrGrapheR')
data('dragons_cgr', package = 'CorrGrapheR')

## ----cars, message=FALSE------------------------------------------------------
library('CorrGrapheR')
library('magrittr')
df <- as.data.frame(datasets::Seatbelts)[,-8] # Drop the binary variable
corrgrapher(df) %>%
  plot()

## ----titanic, eval=FALSE, include=FALSE---------------------------------------
#  library("randomForest")
#  
#  titanic <- na.omit(titanic)
#  model_titanic_rf <- randomForest(survived == "yes" ~ .,
#                                   data = titanic)
#  explain_titanic_rf <- DALEX::explain(model_titanic_rf,
#                                data = titanic[,-9],
#                                y = titanic$survived == "yes",
#                                label = "Random Forest")
#  feature_importance_titanic_rf <- ingredients::feature_importance(explain_titanic_rf)
#  
#  corrgrapher(explain_titanic_rf, feature_importance = feature_importance_titanic_rf) %>%
#    plot()

## ----fifa_show, eval=FALSE----------------------------------------------------
#  library("gbm")
#  
#  data('fifa20', package = 'CorrGrapheR')
#  fifa20_selected <- fifa20[,c(4,5,7,8,11:13,17,25:26,45:78)]
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
#  # Create table with feature importance info - not necessary
#  
#  fifa_feat <- ingredients::feature_importance(fifa_gbm_exp)
#  
#  # Finally, create a corrgrapher object
#  fifa_cgr <- corrgrapher(fifa_gbm_exp, cutoff = 0.4, feature_importance = fifa_feat)

## ----fifa_plot----------------------------------------------------------------
fifa_cgr

## ----dragons_setup, eval=FALSE------------------------------------------------
#  library(randomForest)
#  data(dragons, package = 'DALEX')
#  dragons_rf <- randomForest::randomForest(colour ~ ., data = dragons, num.trees = 50)
#  dragons_rf_exp <- DALEX::explain(dragons_rf, data = dragons[,-5], y = dragons$colour)
#  dragons_feat <- ingredients::feature_importance(dragons_rf_exp, type = 'raw', loss_function = loss_cross_entropy)
#  dragons_cgr <- corrgrapher(dragons_rf_exp, feature_importance = dragons_feat)

## ----dragons_plot-------------------------------------------------------------
dragons_cgr

## ----html, eval=FALSE---------------------------------------------------------
#  ## NOT RUN
#  generate_html(fifa_cgr)

