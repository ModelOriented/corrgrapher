## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(CorrGrapheR)
library(dplyr)

## ----cars---------------------------------------------------------------------
df <- as.data.frame(datasets::Seatbelts) %>%
  select(-law)
plot_corrgraph(df)

## ----fifa---------------------------------------------------------------------
load(normalizePath('../data/fifa20.rda'))
fifa20_selected <- fifa20[,c(4,5,7,8,11:13,17,25:26,45:78)]
plot_corrgraph(fifa20_selected, cutoff = 0.3)

