derivative = function(model, data, feature,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...){

  assertFunction(predict.fun, args = c("object", "newdata"))

  f = function(x, model, data, feature, predict.fun) {
    newdata = replace(data, list = which(colnames(data) == feature), values = x)
    predict.fun(model, newdata = newdata)
  }

  if (is.numeric(data[[feature]])) {
    # calculate numerical derivative
    out = numDeriv::grad(func = f, x = data[[feature]],
      model = model, data = data, feature = feature, predict.fun = predict.fun, ...)
  } else if (is.factor(data[[feature]])) {
    # calculate discrete differences
    lvl = levels(data[[feature]])
    out = setNames(lapply(lvl, function(lev) {
      f(lev, model = model, data = data, feature = feature, predict.fun = predict.fun)
    }), lvl)
  }

  return(out)
}
