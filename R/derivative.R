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
#' @export
derivative = function(x, feature, data, model,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...) {
  # FIXME: currently we only use x as vector of length nrow(data), x of length 1 is necessary for partial dependence derivatives
  assert(checkVector(x, len = 1), checkVector(x, len = nrow(data)))
  assertChoice(feature, colnames(data))
  assertClass(x, class(data[[feature]]))
  assertFunction(predict.fun, args = c("object", "newdata"))
  UseMethod("derivative")
}

#' @export
derivative.numeric = function(x, feature, data, model,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...) {
  # calculate numerical derivative
  out = numDeriv::grad(func = predictModifiedData, x = x,
    feature = feature, data = data, model = model, predict.fun = predict.fun, ...)
  return(out)
}

#' @export
derivative.factor = function(x, feature, data, model,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...) {
  lvls = levels(x)
  out = setNames(lapply(lvls, function(level) {
    predictModifiedData(x = factor(level, levels = lvls), feature = feature, data = data,
      model = model, predict.fun = predict.fun)
  }), lvls)
  return(out)
}

#' @export
derivative.logical = function(x, feature, data, model,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...) {
  lvls = c(FALSE, TRUE)
  out = setNames(lapply(lvls, function(level) {
    predictModifiedData(x = level, feature = feature, data = data,
      model = model, predict.fun = predict.fun)
  }), lvls)
  return(out)
}

#' @export
derivative.character = function(x, feature, data, model,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...) {
  lvls = unique(x)
  out = setNames(lapply(lvls, function(level) {
    predictModifiedData(x = level, feature = feature, data = data,
      model = model, predict.fun = predict.fun)
  }), lvls)
}

# Modify feature in data set and predict using this modified data
predictModifiedData = function(x, feature, data, model, predict.fun) {
  #newdata = replace(data, list = which(colnames(data) == feature), values = x)
  newdata = replace(data, feature, values = x)
  p = predict.fun(model, newdata = newdata)
  if (length(x) == 1) mean(p) else p
}
