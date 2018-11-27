# Average Marginal Effects

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/ame)](http://cran.r-project.org/web/packages/ame)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/ame)](http://cran.rstudio.com/web/packages/ame/index.html)
[![Build Status](https://travis-ci.org/compstat-lmu/ame.svg?branch=master)](https://travis-ci.org/compstat-lmu/ame.svg?branch=master)
[![codecov](https://codecov.io/gh/compstat-lmu/ame/branch/master/graph/badge.svg)](https://codecov.io/gh/compstat-lmu/ame)

Marginal Effects represent marginal rates of change of a prediction function at specified covariate values. They're an intuitive way of describing the slope of a prediction function and
easily computed by numerically differentiating it at either observed or manually set values of the feature space. `ame` is the first software package written in R to implement Marginal Effects for any kind of prediction model.

# Theory

The Marginal Effect [ME] for numeric variables is the first derivative of a model $f(x_S, x_C)$ with respect to the selected feature $x_S$ at specified values of the covariates. $x_C$ represents all unselected features.
$$
\begin{gather*}
ME(x_S) = \frac{\partial f(x_S, x_C)}{\partial x_S}
\end{gather*}
$$

For factor variables the ME represents the change in $f(x_S, x_C)$ for a single category of $x_S$ compared to a base category.

The ME for numeric features is estimated by numerically differentiating the fitted prediction function $\hat{f}(x_S, x_C)$ with respect to the selected feature $x_S$ at specified values of the covariates.

$$
\begin{gather*}
\widehat{ME}(x_S) = \widehat{Gradient}_{x_S}\left[ \hat{f}(x_S, x_C)\right]
\end{gather*}
$$

The ME for factor features is estimated by comparing the change of $\hat{f}(x_S, x_C)$ compared to a base category while $x_C$ is held constant and $x_S$ is being replaced by a category other than the base category.

There are three kinds of Marginal Effects. The `ame` package supports all of them:

- Average Marginal Effects [AME]: The average slope of the prediction function at observed covariate values.
- Marginal Effects at the Means [MEM]: The average slope of the prediction function while the unselected features $x_C$ are set to their sample means.
- Marginal Effects at Representative Values [MER]: The slope of the prediction function while one or multiple features in $x_C$ is/are set to manually specified values.

# Installation of the package

Install the development version from GitHub (using `devtools`)

```r
devtools::install_github("compstat-lmu/ame")
```
# Using the package

For regression targets the computation of AME's is straightforward:

```r
library(mlr)
library(ame)
df = getTaskData(bh.task)
mod = train("regr.svm", bh.task)
computeAME(mod, "age", data = df)

mod.glm = glm(medv ~ . , family = Gamma, data = df)
computeAME(mod, "age", data = df)
```

For classification tasks, the classification probabilites for a specified target class are differentiated. Providing the right prediction function is essential in order to receive the correct results. The prediction function is a function argument of `computeAME()` and needs to be chosen depending on the underlying model. We highly encourage to test the prediction functions in advance. A prediction function consists of the following structure and has a fitted model and a data frame as arguments:

```r
pred.fun = function(object, newdata) {
  predict(object, newdata = newdata)
}
```

We provide an example of a multinomial model, fitted on the iris dataset with the `multinom` function of the `nnet` package, as well as `mlr` and `caret`. We're interested in the Average Marginal Effect of `Petal.Width` on the probability of classifying an instance as `Virginica`. The prediction functions are set up such that they return the probabilites of classifying an instance as `Virginica`:

```r
library(mlr)
library(caret)
library(nnet)

tsk = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.multinom", predict.type = "prob")

mod.multinom = nnet::multinom(Species ~ ., data = iris)
mod.caret = caret::train(Species ~ ., data = iris, method = "multinom")
mod.mlr = mlr::train(lrn, tsk)

predict.fun.multinom = function(object, newdata) {
  probs = predict(object, newdata = newdata, type = "prob")
  return(probs[, "virginica"])}

predict.fun.caret = function(object, newdata) {
  probs = predict(object, newdata = newdata, type = "prob")
  return(probs[, "virginica"])}

predict.fun.mlr = function(object, newdata) {
  probs = getPredictionProbabilities(
    predict(object, newdata = newdata)
  )
  return(probs[, "virginica"])
}

computeAME(mod.multinom, data = iris, features = "Petal.Width",
           predict.fun = predict.fun.multinom)

computeAME(mod.caret, data = iris, features = "Petal.Width",
           predict.fun = predict.fun.caret)

computeAME(mod.mlr, data = iris, features = "Petal.Width",
           predict.fun = predict.fun.mlr)
```



