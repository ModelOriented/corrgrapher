## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(CorrGrapheR)
library(dplyr)
#.

## ----cars---------------------------------------------------------------------
df <- as.data.frame(datasets::Seatbelts) %>%
  select(-law)
create_corrgrapher(df) %>%
  plot()

## ----fifa---------------------------------------------------------------------
load(normalizePath('../data/fifa20.rda'))
fifa20_selected <- fifa20[,c(4,5,7,8,11:13,17,25:26,45:78)]
# Value is skewed. Will be much easier to model sqrt(Value).

fifa20_selected$value_eur <- log10(fifa20_selected$value_eur)
fifa20_selected$team_position <- factor(fifa20_selected$team_position)
fifa20_selected <- na.omit(fifa20_selected)
fifa20_selected <- fifa20_selected[fifa20_selected$value_eur > 0,]
fifa20_selected <- fifa20_selected[!duplicated(fifa20_selected[,1]),]
rownames(fifa20_selected) <- fifa20_selected[,1]
fifa20_selected <- fifa20_selected[,-1]

# create a gbm model

set.seed(1313)
library("gbm")
# 4:5 are overall and potential, too strong predictors
fifa_gbm <- gbm(value_eur ~ . , data = fifa20_selected[,-(4:5)], n.trees = 250, interaction.depth = 3)

library("DALEX")
fifa_gbm_exp <- explain(fifa_gbm, 
                        data = fifa20_selected[, -6], 
                        y = 10^fifa20_selected$value_eur, 
                        predict_function = function(m,x) 
                          10^predict(m, x, n.trees = 250))

library("ingredients")
fifa_feat <- ingredients::feature_importance(fifa_gbm_exp)

create_corrgrapher(fifa20_selected, cutoff = 0.3) %>%
  plot()

