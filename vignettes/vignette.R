## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(CorrGrapheR)
library(dplyr)

## ----cars---------------------------------------------------------------------
df <- as.data.frame(datasets::Seatbelts) %>%
  select(-law)
plot_corrgraph(df)

