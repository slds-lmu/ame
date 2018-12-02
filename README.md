# Average Marginal Effects

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/ame)](http://cran.r-project.org/web/packages/ame)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/ame)](http://cran.rstudio.com/web/packages/ame/index.html)
[![Build Status](https://travis-ci.org/compstat-lmu/ame.svg?branch=master)](https://travis-ci.org/compstat-lmu/ame.svg?branch=master)
[![codecov](https://codecov.io/gh/compstat-lmu/ame/branch/master/graph/badge.svg)](https://codecov.io/gh/compstat-lmu/ame)

Marginal Effects represent marginal rates of change of a prediction function at specified covariate values. They're an intuitive way of describing the slope of a prediction function and
easily computed by numerically differentiating it at either observed or manually chosen values of the feature space. `ame` is the first software package written in R to implement Marginal Effects for any kind of prediction model.

# Theory

<!The Marginal Effect [ME] for numeric variables is the first derivative of a model $f(x_S, x_C)$ with respect to the selected feature $x_S$ at specified values of the covariates. $x_C$ represents all unselected features.

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
>


The Marginal Effect [ME] for numeric variables is the first derivative of a model with respect to the selected feature at specified values of the covariates. It is estimated by numerically differentiating the fitted prediction function. The ME for factorial features is estimated for each category by comparing the change in prediction compared to a base category while all other variables are held constant.

There are three kinds of Marginal Effects. The `ame` package supports all of them:

- Average Marginal Effects [AME]: The average slope of the prediction function at observed covariate values.
- Marginal Effects at the Means [MEM]: The average slope of the prediction function while the values of unselected features are set to their sample means.
- Marginal Effects at Representative Values [MER]: The slope of the prediction function while other features are set to manually specified values.

# Installation of the package

Install the development version from GitHub (using `devtools`)

```r
devtools::install_github("compstat-lmu/ame")
```
# Using the package

We start with the computation of AME's. For regression tasks the computation of AME's is straightforward:

```r
library(mlr)
library(ame)
library(e1071)
df = getTaskData(bh.task)

mod.svm = e1071::svm(medv ~ . , data = df, kernel = "radial")
computeAME(mod.svm, "age", data = df)

lrn = makeLearner("regr.svm", par.vals = list(kernel = "radial"))
mod.mlr = mlr::train(lrn, bh.task)
computeAME(mod, "age", data = df)

mod.caret = caret::train(medv ~ . , data = df, "svmRadial")
computeAME(mod.caret, "age", data = df)
```

For classification tasks, the classification probabilites for a specified target class are differentiated. Providing the right prediction function is essential in order to receive the correct results. The prediction function is a function argument of `computeAME()` and needs to be chosen depending on the underlying model. We highly encourage to test the prediction functions in advance. A prediction function consists of the following structure and has a fitted model and a data frame as arguments:

```r
pred.fun = function(object, newdata) {
  predict(object, newdata = newdata)
}
```

We provide an example of a multinomial model, fitted on the iris dataset with the `multinom` function of the `nnet` package, as well as with `mlr` and `caret`. We're interested in the Average Marginal Effect of `Petal.Width` on the probability of classifying an instance as `Virginica`. The prediction functions are set up such that they return the probabilites of classifying an instance as `Virginica`:

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

The AME can also be disaggregated by passing a different aggregation function to
`computeAME()`:

```r
computeAME(mod.mlr, data = iris, features = "Petal.Width",
           predict.fun = predict.fun.mlr, aggregate.fun = identity)
```

Other features can be fixed at specified values, which results in MER's.
They can be regarded as conditional ME's:

```r
at = list("rm" = c(3, 4, 5, 6, 7, 8))
computeAME(mod.caret, "age", data = df, aggregate.fun = mean, at = at)
```

By setting a factorial feature to different values we can look at counterfactuals:

```r
at = list("chas" = "0")
computeAME(mod.caret, "age", data = df, aggregate.fun = mean, at = at)
```

Computation of MEM's is possible, although not recommended. The sample means 
might not be representative of the population means:

```
feature.list = colnames(df)[!colnames(df) %in% c("chas", "medv")]
sample.means = lapply(feature.list, FUN = function(feature) {
  mean(df[, feature])
})
names(sample.means) = feature.list

computeAME(mod.caret, "age", data = df, aggregate.fun = mean, at = sample.means)
```
