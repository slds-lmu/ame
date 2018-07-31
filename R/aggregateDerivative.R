aggregateDerivative = function(x, feature, data, model,
  predict.fun = function(object, newdata) predict(object, newdata = newdata),
  aggregate.fun = mean, ...) {
  assertFunction(aggregate.fun)

  effect = derivative(x, feature, data, model, predict.fun, ...)

  if (is.numeric(x)) {
    return(setNames(aggregate.fun(effect), feature))
  } else if (is.factor(x)) {
    # n = rep(nrow(data), 2)
    # mp = marginalPrediction(data = data, vars = feat, n = n, model = model,
    #   uniform = FALSE, predict.fun = predict.fun)
    # reflev = levels(mp[[feat]])[1]
    # effect = mp$preds - mp$preds[mp[[feat]] == reflev]
    # effect = setNames(effect, paste0(feat, mp[[feat]]))
    # effect = effect[effect != 0]
    # FIXME: make reflev changable from outside
    reflev = levels(x)[1]
    effect = vnapply(effect, aggregate.fun)
    effect = effect[names(effect) != reflev] - effect[names(effect) == reflev]
    return(setNames(effect, paste0(feature, names(effect))))
  } else if (is.character(x)) {
    reflev = sort(unique(x))[1]
    effect = unlist(effect)
    effect = effect[names(effect) != reflev] - effect[names(effect) == reflev]
    return(setNames(effect, paste0(feature, names(effect))))
  } else if (is.logical(x)) {
    return(setNames(effect[["TRUE"]] - effect[["FALSE"]], paste0(feature, "TRUE")))
  }
}
