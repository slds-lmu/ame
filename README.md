# Average Marginal Effects

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/ame)](http://cran.r-project.org/web/packages/ame)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/ame)](http://cran.rstudio.com/web/packages/ame/index.html)
[![Build Status](https://travis-ci.org/compstat-lmu/ame.svg?branch=master)](https://travis-ci.org/compstat-lmu/ame.svg?branch=master)
[![codecov](https://codecov.io/gh/compstat-lmu/ame/branch/master/graph/badge.svg)](https://codecov.io/gh/compstat-lmu/ame)

Marginal Effects represent marginal rates of change of a prediction function at specified covariate values. ME's are an intuitive way of describing the shape of a prediction function and
easily computed by numerically differentiating it at either observed or manually set values of the feature space. 'ame' is the first software package written in R to implement Marginal Effects for any prediction model.

# Installation of the package

Install the development version from GitHub (using `devtools`)

```r
devtools::install_github("compstat-lmu/ame")
```
