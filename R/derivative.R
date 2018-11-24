#' Computes the derivative of a feature
#'
#' Computes the derivative of a feature at point or vector x.
#'
#' @param x [\code{vector}]\cr
#'   A scalar value or vector indicating the point(s) at which the gradient is to be calculated.
#' @param feature [\code{character(1)}]\cr
#'   The column name of the data set that refers to the feature for which the derivative will be computed.
#' @template arg_data
#' @template arg_model
#' @template arg_predict.fun
#' @param ...
#'   Further options passed down to the \code{\link[numDeriv]{grad}} function.
#'
derivative = function(x, feature, data, model,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...) {
  # FIXME: currently we only use x as vector of length nrow(data), x of length 1 is necessary for partial dependence derivatives
  assert(checkVector(x, len = 1), checkVector(x, len = nrow(data)))
  assertChoice(feature, colnames(data))
  assertClass(x, class(data[[feature]]))
  assertFunction(predict.fun, args = c("object", "newdata"))
  UseMethod("derivative")
}

derivative.numeric = function(x, feature, data, model,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...) {
  # calculate numerical derivative
  out = numDeriv::grad(func = predictModifiedData, x = x,
    feature = feature, data = data, model = model, predict.fun = predict.fun, ...)
  return(out)
}

derivative.factor = function(x, feature, data, model,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...) {
  lvl = levels(x)
  out = setNames(lapply(lvl, function(lev) {
    predictModifiedData(x = lev, feature = feature, data = data,
      model = model, predict.fun = predict.fun)
  }), lvl)
  return(out)
}

derivative.logical = function(x, feature, data, model,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...) {
  lvl = unique(x)
  out = setNames(lapply(lvl, function(lev) {
    predictModifiedData(x = lev, feature = feature, data = data,
      model = model, predict.fun = predict.fun)
  }), lvl)
  return(out)
}

derivative.character = function(x, feature, data, model,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...) {
  x = as.factor(x) #droplevels(factor(x, levels = levels(data[[feature]])))
  data[[feature]] = as.factor(data[[feature]])
  derivative(x, feature, data, model, predict.fun, ...)
}

# Modify feature in data set and predict using this modified data
predictModifiedData = function(x, feature, data, model, predict.fun) {
  if (is.numeric(data[[feature]])) {
    prediction.modifdata = predictModifiedDataNumeric(
      x, feature, data, model, predict.fun)
  } else {
    prediction.modifdata = predictModifiedDataFactor(
      x, feature, data, model, predict.fun)
  }
  return(prediction.modifdata)
}

predictModifiedDataNumeric = function(x, feature, data, model, predict.fun) {
  # modify data and return prediction in case of a numeric feature
  newdata = replace(data, list = which(colnames(data) == feature), values = x)
  p = predict.fun(model, newdata = newdata)
  if (length(x) == 1) mean(p) else p
}

predictModifiedDataFactor = function(x, feature, data, model, predict.fun) {
  # modify data and return prediction in case of a factor feature
  newdata = data
  feature.levels = levels(data[[feature]])
  newdata[[feature]] = factor(x = x, levels = feature.levels)
  p = predict.fun(model, newdata = newdata)
  return(p)
}
