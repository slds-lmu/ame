compAMEFeature = function(x, object, feature, data, predict.fun, aggr.fun, delta) {
  UseMethod("compAMEFeature")
}

compAMEFeature.default = function(x, object, feature, ...) {
  warning("Data type of ", feature, " is unsupported.")
  return(NA)
}

compAMEFeature.numeric = function(x, object, feature, data, predict.fun, aggr.fun, delta) {
  data.delta = data
  data.delta[, feature] = data.delta[, feature] + delta
  prediction = predict.fun(object, data)
  prediction.delta = predict.fun(object, data.delta)
  return(aggr.fun((prediction.delta - prediction) / delta))
}

compAMEFeature.factor = function(x, object, feature, data, predict.fun, aggr.fun, delta) {
  lvls = levels(x) # first level is taken as reference
  data[, feature] = factor(lvls[1], levels = lvls)
  prediction.reference = predict.fun(object, data)
  data.lvl = data
  ame = vapply(lvls[-1], FUN = function(level) {
    data.lvl[, feature] = factor(level, levels = lvls)
    prediction.lvl = predict.fun(object, data.lvl)
    aggr.fun(prediction.lvl - prediction.reference) # support alternative aggr functions
  }, FUN.VALUE = NA_real_)
  return(setNames(ame, paste(feature, names(ame), sep = ".")))
}

compAMEFeature.logical = function(x, object, feature, data, predict.fun, aggr.fun, delta) {
  data.true = data
  data[, feature] = FALSE
  data.true[, feature] = TRUE
  prediction.false = predict.fun(object, data)
  prediction.true = predict.fun(object, data.true)
  ame = aggr.fun(prediction.true - prediction.false) # support alternative aggr functions
  return(setNames(ame, paste(feature, "TRUE", sep = ".")))
}

compAMEFeature.character = function(x, object, feature, data, predict.fun, delta) {
  # same as .factor, but x has to be character for prediction()
  lvls = unique(x) # first level is taken as reference
  data[, feature] = factor(lvls[1], levels = lvls)
  prediction.reference = predict.fun(object, data)
  data.lvl = data
  ame = vapply(lvls[-1], FUN = function(level) {
    data.lvl[, feature] = factor(level, levels = lvls)
    prediction.lvl = predict.fun(object, data.lvl)
    aggr.fun(prediction.lvl - prediction.reference) # support alternative aggr functions
  }, FUN.VALUE = NA_real_)
  return(setNames(ame, paste(feature, names(ame), sep = ".")))
}
