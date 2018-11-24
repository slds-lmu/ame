# internal helper functions
computeAMEInternal = function(model, data, features,
  predict.fun = NULL, aggregate.fun = mean, cl = NULL, ...){
  UseMethod("computeAMEInternal")
}

computeAMEInternal.train = function(model, data, features,
  predict.fun = NULL, aggregate.fun = mean, cl = NULL, ...){
  tt = model$modelType
  # FIXME: support multiclass
  if (is.null(predict.fun)) {
    if (tt == "Classification") {
      if (is.null(cl)) cl = model$finalModel$obsLevels[1]
      predict.fun = function(object, newdata)
        predict(object, newdata = newdata, type = "prob")[[cl]]
    } else if (tt == "Regression") {
      # FIXME: check if this works
      predict.fun = function(object, newdata)
        predict(object, newdata = newdata, type = "raw")
    }
  }
  computeAMEInternal.default(model, data, features, predict.fun = predict.fun,
    aggregate.fun = aggregate.fun, cl = cl, ...)
}

computeAMEInternal.WrappedModel = function(model, data, features,
  predict.fun = NULL, aggregate.fun = mean, cl = NULL, ...) {
  # FIXME: allow data to be also a mlr task
  # FIXME: support multiclass
  tt = mlr::getTaskType(model)
  if (is.null(cl))
    cl = mlr::getTaskDesc(model)$positive
  if (is.null(predict.fun)) {
    if (tt == "classif") {
      predict.fun = function(object, newdata)
        mlr::getPredictionProbabilities(predict(object, newdata = newdata), cl = cl)
    } else if (tt == "regr") {
      predict.fun = function(object, newdata)
        mlr::getPredictionResponse(predict(object, newdata = newdata))
    }
  }
  computeAMEInternal.default(model, data, features, predict.fun = predict.fun,
    aggregate.fun = aggregate.fun, cl = cl, ...)
}

computeAMEInternal.default = function(model, data, features,
  predict.fun = NULL,
  aggregate.fun = mean,
  cl = NULL, ...){
  if (!is.null(cl))
    messagef("'cl' will be ignored for '%s' model objects use 'predict.fun' to change the target class level.", class(model)[1])
  # FIXME: ensure that data frame has only numerical, factors or logical features
  if (is.null(predict.fun))
    predict.fun = function(object, newdata) predict(object, newdata = newdata)
  # FIXME: do this in parallel
  ame = lapply(features, function(feat) {
    x = data[[feat]]
    effect = aggregateDerivative(x = x, feature = feat, data, model,
      predict.fun = predict.fun, aggregate.fun = aggregate.fun, ...)
  })
  # ame = addClasses(ame, "AME")
  return(ame)
}
