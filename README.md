# corrgrapher <img src="man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->
[![R build status](https://github.com/ModelOriented/corrgrapher/workflows/R-CMD-check/badge.svg)](https://github.com/ModelOriented/corrgrapher/actions?query=workflow%3AR-CMD-check)

[![Travis build status](https://travis-ci.org/ModelOriented/corrgrapher.svg?branch=master)](https://travis-ci.org/ModelOriented/corrgrapher)
[![Codecov test coverage](https://codecov.io/gh/ModelOriented/corrgrapher/branch/master/graph/badge.svg)](https://codecov.io/gh/ModelOriented/corrgrapher?branch=master)
<!-- badges: end -->

## The problem with pairs

Data analysis (and creating ML models) involves many stages. For early exploration, it is useful to have a grip not only on individual series (AKA variables) available, but also on relations between them. Unfortunately, the task of understanding correlations between variables proves to be difficult (`n`variables means `n(n-1)/2` pairs of variables). Furthermore, the mainstream method of visualizing them (i.e. correlation matrix) has its limits; the more variables, the less readable (and therefore meaningful) it becomes.  

## The solution - less numbers, more layout

This package aims to plot correlations between variables in form of a graph. Each node on it is associated with single variable. Variables correlated with each other (positively and negatively alike) shall be close, and weakly correlated  - far from each other. 

It is achieved through a physical simulation, where the nodes are treated as points with mass (and are pushing each other away) and edges are treated as mass-less springs. The length of a spring depends on absolute value of correlation between connected nodes. The bigger the correlation, the shorter the spring.

## Installation

The easiest way to get `corrgrapher` is to install it from CRAN:

```{r}
install.packages("corrgrapher")
```

Or the the development version from GitHub:

```{r}
devtools::install_github("ModelOriented/corrgrapher")
```

## Example of use

```
library('corrgrapher')
df <- as.data.frame(datasets::Seatbelts)[,-8] # Drop the binary variable
cgr <- corrgrapher(df)
cgr
```

## See also

Feel free to take a look at articles about
[introduction](https://modeloriented.github.io/corrgrapher/articles/Introduction.html) and [customization](https://modeloriented.github.io/corrgrapher/articles/Customization.html) at
[package site](https://modeloriented.github.io/corrgrapher/index.html).
