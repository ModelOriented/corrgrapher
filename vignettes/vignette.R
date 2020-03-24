## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
load('../data/fifa_gbm_exp.rda')
load('../data/fifa_feat.rda')
#.

## ----cars, message=FALSE------------------------------------------------------
library('CorrGrapheR')
library('dplyr')
df <- as.data.frame(datasets::Seatbelts) %>%
  select(-law)
create_corrgrapher(df) %>%
  plot()

## ----titanic, cache=TRUE, eval=FALSE, include=FALSE---------------------------
#  library("randomForest")
#  
#  titanic <- na.omit(titanic)
#  model_titanic_rf <- randomForest(survived == "yes" ~ .,
#                                   data = titanic)
#  explain_titanic_rf <- explain(model_titanic_rf,
#                                data = titanic[,-9],
#                                y = titanic$survived == "yes",
#                                label = "Random Forest")
#  feature_importance_titanic_rf <- feature_importance(explain_titanic_rf)
#  
#  create_corrgrapher(explain_titanic_rf, feature_importance = feature_importance_titanic_rf) %>%
#    plot()

## ----fifa_show, eval=FALSE----------------------------------------------------
#  library("gbm")
#  
#  load(normalizePath('../data/fifa20.rda'))
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
#  library("DALEX")
#  fifa_gbm_exp <- explain(fifa_gbm,
#                          data = fifa20_selected[, -6],
#                          y = 10^fifa20_selected$value_eur,
#                          predict_function = function(m,x)
#                            10^predict(m, x, n.trees = 250))
#  
#  # Create table with feature importance info - not necessary
#  
#  library("ingredients")
#  fifa_feat <- feature_importance(fifa_gbm_exp)
#  

## ----fifa_plot----------------------------------------------------------------
create_corrgrapher(fifa_gbm_exp, cutoff = 0.4, feature_importance = fifa_feat) %>%
  plot(width = 800, height = 600)

