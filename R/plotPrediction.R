#' Plot feature values against predictions of a model
#'
#' Creates univariate plots of a feature's values against model predictions.
#' Supports smoothing using loess regression.
#'
#' @section TODO:
#' \itemize{
#'   \item loess smoothing
#'   \item implement mlr models
#'   \item support multiple features at once
#' }
#'
#'
#' @param model
#' @param data / task
#' @param feature
#' @
#'
#' @return
#' @export
#'
#' @examples
plotPrediction = function(model, ...) {
  UseMethod("plotPrediction")
}

#' @export
plotPrediction.default = function(model, data, feature, predict.fun = predict, plot.points = FALSE) {
  x = data[, feature]
  y.hat = predict.fun(model, newdata = data)
  plot.data = data.frame(x, y.hat)
  p = ggplot(data = plot.data, aes(x = x, y = y.hat)) + geom_line() + xlab(feature)
  if (plot.points) p + geom_point()
  return(list(plot = p, plot.data = plot.data))
}

#' @export
plotPrediction.WrappedModel = function(model, task, feature, plot.points = FALSE) {
  x = getTaskData(task)[, feature]
  y.hat = getPredictionResponse(predict(model, task = task))
  plot.data = data.frame(x, y.hat)
  p = ggplot(data = plot.data, aes(x = x, y = y.hat)) + geom_line() + xlab(feature)
  if (plot.points) p + geom_point()
  return(list(plot = p, plot.data = plot.data))
}
