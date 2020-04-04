## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE)
data('fifa_cgr', package = 'CorrGrapheR')

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
#  fifa_pd <- ingredients::partial_dependency(fifa_gbm_exp)
#  # Finally, create a corrgrapher object
#  fifa_cgr <- corrgrapher(fifa_gbm_exp, cutoff = 0.4,
#                          feature_importance = fifa_feat,
#                          partial_dependency = fifa_pd)

## ----fifa_plot----------------------------------------------------------------
fifa_cgr

## ----dragons_setup------------------------------------------------------------
library(ranger)
data(dragons, package='DALEX')
model <- ranger::ranger(colour ~ ., data = dragons, num.trees = 100, probability = TRUE)
model_exp <- DALEX::explain(model, data = dragons[,-5], y = dragons$colour)
model_fi <- ingredients::feature_importance(model_exp, 
                                            loss_function = DALEX::loss_accuracy, 
                                            type = 'raw')
model_pd <- ingredients::partial_dependence(model_exp, N=100, grid_points = 81)

dragons_cgr <- corrgrapher(model_exp, 
                       feature_importance = model_fi,
                       partial_dependency = model_pd)

## ----dragons_plot-------------------------------------------------------------
dragons_cgr

## ----html, eval=FALSE---------------------------------------------------------
#  ## NOT RUN
#  save_cgr_to_html(fifa_cgr)

