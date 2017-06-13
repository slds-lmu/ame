# internal helper functions
computeAMEInternal = function(model, data, features, predict.fun = NULL, ...){
  UseMethod("computeAverageMarginalEffects")
}

computeAMEInternal.WrappedModel = function(model, data, features, predict.fun = NULL, ...){
  # FIXME: support multiclass
  tt = getTaskType(model)
  if (is.null(predict.fun)) {
    if (tt == "classif") {
      predict.fun = function(object, newdata)
        mlr::getPredictionProbabilities(predict(object, newdata = newdata))
    } else if (tt == "regr") {
      predict.fun = function(object, newdata)
        mlr::getPredictionResponse(predict(object, newdata = newdata))
    }
  }
  computeAMEInternal.default(model, data, features, predict.fun = predict.fun, ...)
}

computeAMEInternal.default = function(model, data, features,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...){
  # FIXME: ensure that data frame has only numerical, factors or logical features

  ame = lapply(features, function(feat) {
    x = data[[feat]]
    effect = aggregateDerivative(x = x, feature = feat, data, model,
      predict.fun = predict.fun, aggregate.fun = mean, ...)
  })
  # ame = addClasses(ame, "AME")
  return(ame)
}

# FIXME: support caret
