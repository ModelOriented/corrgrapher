## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(CorrGrapheR)
library(dplyr)
library("ingredients")
library("DALEX")
library("gbm")
#.

## ----cars---------------------------------------------------------------------
df <- as.data.frame(datasets::Seatbelts) %>%
  select(-law)
create_corrgrapher(df) %>%
  plot()

## ----fifa_plot----------------------------------------------------------------
create_corrgrapher(fifa_gbm_exp, cutoff = 0.4, feature_importance = fifa_feat) %>%
  plot()

