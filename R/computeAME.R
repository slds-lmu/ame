#' @title Compute Average Marginal Effects
#'
#' @description Computes the average marginal effects for specified features.
#'
#' @param model [\code{\link[mlr]{WrappedModel}} | \code{any}]\cr
#'   A model object. Can be also an object of class \code{\link[mlr]{WrappedModel}}.
#' @param data [\code{logical(1)}]\cr
#'   The data set that was used to fit the model.
#' @param features [\code{logical(1)}]\cr
#'   The features for which the average marginal effects should be computed.
#' @param ...
#'   Further options passed down to the \code{\link[numDeriv]{grad}} function.
#'
#' @export
computeAME = function(model, data, features, ...){
  UseMethod("computeAME")
}

computeAME.WrappedModel = function(model, data, features, ...){
  # FIXME: support multiclass
  tt = getTaskType(model)
  if (tt == "classif") {
    computeAME.default(model, data, features,
      predict.fun = function(object, newdata)
        getPredictionProbabilities(predict(object, newdata = newdata)), ...)
  } else if (tt == "regr") {
    computeAME.default(model, data, features,
      predict.fun = function(object, newdata)
        getPredictionResponse(predict(object, newdata = newdata)), ...)
  }
}

computeAME.default = function(model, data, features,
  predict.fun = function(object, newdata) predict(object, newdata = newdata), ...){
  # FIXME: ensure that data frame has only numerical or factor features

  n = rep(nrow(data), 2)
  ame = lapply(features, function(feat) {
    if (is.numeric(data[[feat]])) {
     effect = derivative(model, data, feature = feat, predict.fun = predict.fun, ...)
     return(setNames(mean(effect), feat))
    } else if (is.factor(data[[feat]])) {
      # mp = marginalPrediction(data = data, vars = feat, n = n, model = model,
      #   uniform = FALSE, predict.fun = predict.fun)
      # reflev = levels(mp[[feat]])[1]
      # effect = mp$preds - mp$preds[mp[[feat]] == reflev]
      # effect = setNames(effect, paste0(feat, mp[[feat]]))
      # effect = effect[effect != 0]
      # FIXME: make reflev changable from outside
      reflev = levels(data[[feat]])[1]
      effect = derivative(model, data, feature = feat, predict.fun = predict.fun, ...)
      effect = vnapply(effect, mean)
      effect = effect[names(effect) != reflev] - effect[names(effect) == reflev]
      return(setNames(effect, paste0(feat, names(effect))))
    }
  })
  ame = addClasses(ame, "AME")
  return(ame)
}
