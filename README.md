<!-- badges: start -->
[![Travis build status](https://travis-ci.org/ModelOriented/CorrGrapheR.svg?branch=master)](https://travis-ci.org/ModelOriented/CorrGrapheR)
[![Codecov test coverage](https://codecov.io/gh/ModelOriented/CorrGrapheR/branch/master/graph/badge.svg)](https://codecov.io/gh/ModelOriented/CorrGrapheR?branch=master)
<!-- badges: end -->
# CorrGrapheR
Visualize correlations between variables in datasets

## The problem with pairs

Data analysis (and creating ML models) involves many stages. For early exploration, it is useful to have a grip not only on individual series (AKA variables) available, but also on relations between them. Unfortunately, the task of understanding corelations between variables proves to be difficult ($n$ variables means $n(n-1) / 2$ pairs of variables). Furthermore, the mainstream method of visualizing them (i.e. correlation matrix) has its limits; the more variables, the less readable (and therefore meaningful) it becomes.  

## The solution - less numbers, more layout

This package aims to plot correlations between variables in form of a graph. Each node on it is associated with single variable. Variables correlated with each other (posivitely and negatively alike) shall be close, and weakly correlated  - far from each other. 

It is achieved through a physical simulation, where the nodes are treated as points with mass (and are pushing each other away) and edges are treated as mass-less springs. The length of a spring depends on absolute value of correlation between connected nodes. The bigger the correlation, the shorter the spring.

## Installation

Install the `CorrGrapheR` package from GitHub.

```
devtools::install_github("ModelOriented/CorrGrapheR")
```

## Example of use

```
library('CorrGrapheR')
library('magrittr')
df <- as.data.frame(datasets::Seatbelts)[,-8] # Drop the binary variable
cgr <- corrgrapher(df)
cgr
```

## See also

Feel free to take a look at
[introduction](https://modeloriented.github.io/CorrGrapheR/articles/vignette.html) at
[package site](https://modeloriented.github.io/CorrGrapheR/index.html).