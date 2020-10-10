# corrgrapher <img src="man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/ModelOriented/corrgrapher.svg?branch=master)](https://travis-ci.org/ModelOriented/corrgrapher)
[![Codecov test coverage](https://codecov.io/gh/ModelOriented/corrgrapher/branch/master/graph/badge.svg)](https://codecov.io/gh/ModelOriented/corrgrapher?branch=master)
[![R build status](https://github.com/ModelOriented/corrgrapher/workflows/R-CMD-check/badge.svg)](https://github.com/ModelOriented/corrgrapher/actions)
<!-- badges: end -->

## Overview

When exploring data or models we often examine variables one by one. This analysis is incomplete if the relationship between these variables is not taken into account. The corrgrapher package facilitates simultaneous exploration of the Partial Dependence Profiles and the correlation between variables in the model.

The package [corrgrapher](https://github.com/ModelOriented/corrgrapher) is a part of the [DrWhy.AI](http://drwhy.ai/) universe. 

## The solution - less numbers, more insights

This package aims to plot correlations between variables in form of a graph. Each node on it is associated with single variable. Variables correlated with each other (positively and negatively alike) shall be close, and weakly correlated  - far from each other. 

It is achieved through a physical simulation, where the nodes are treated as points with mass (and are pushing each other away) and edges are treated as mass-less springs. The length of a spring depends on absolute value of correlation between connected nodes. The bigger the correlation, the shorter the spring.

When you click on the node of the graph you can view the distribution or the Partial Dependence Plot for the selected variable.

## Installation

The easiest way to get `corrgrapher` is to install it from CRAN:

```{r}
install.packages("corrgrapher")
```

Or the the development version from GitHub:

```{r}
devtools::install_github("ModelOriented/corrgrapher")
```

## Examples

First, load the package

```
library('corrgrapher')
```

### For data sets

For data frames the `corrgrapher` shows correlation network and histograms/distributions for features.

```
df <- as.data.frame(datasets::Seatbelts)
cgr <- corrgrapher(df)
cgr
```

<center>
<img src="https://github.com/ModelOriented/corrgrapher/raw/master/inst/corrgrapher1.gif">
</center>

### For models

For models the `corrgrapher` shows partial dependencies. Use the `DALEX::explain()` function to create an adapter for any predictive model.

```
library(DALEX)
library(ranger)

titanic_rgr <- ranger(survived ~ ., data = titanic_imputed, classification = TRUE)
titanic_exp <- explain(titanic_rgr, data = titanic_imputed, y = titanic_imputed$survived, verbose = FALSE)
cgr <- corrgrapher(titanic_exp)
cgr
```

<center>
<img src="https://github.com/ModelOriented/corrgrapher/raw/master/inst/corrgrapher2.gif">
</center>

## See also

Feel free to take a look at articles about
[introduction](https://modeloriented.github.io/corrgrapher/articles/Introduction.html) and [customization](https://modeloriented.github.io/corrgrapher/articles/Customization.html) at
[package site](https://modeloriented.github.io/corrgrapher/index.html).


## Acknowledgments

Work on this package was financially supported by the Polish National Science Centre under Opus Grant number 2017/27/B/ST6/0130.
