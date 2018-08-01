#' Compute AME given break points on the sample space of a feature
#'
#' @template arg_model
#' @template arg_data
#' @param feature [\code{character(1)}]\cr
#'   Feature name, subset of \code{colnames(data)}.
#' @param breaks [\code{numeric}]
#'
#' @return
#' @export
#'
#' @examples
computeAMEInterval = function(model, data, feature, n.parts = 5, method = "ALE", breaks = NULL, ...) {

  if (is.null(breaks)) {
    if (method == "ALE") {
      ALE = computeALE(model, data, feature, ...)
      breaks = partition(ALE$ale.x, ALE$ale, n.parts)
    } else if (method == "PDeriv") {
      PD = computePD(model = model, data = data, feature = feature, derivative = TRUE, ...)
      breaks = partition(PD$x.grid, PD$y.hat, n.parts)
    }
  }

  x = data[, feature]
  y.hat = predict(model, newdata = data)
  bounds = unique(c(min(x), sort(breaks), max(x) + 0.00001))
  l = length(bounds) - 1
  AME = numeric(l)
  y.hat.mean = numeric(l)
  x.interval.average = numeric(l)
  for (i in 1:l) {
    selection = x >= bounds[i] & x < bounds[i+1]
    data.interval = data[selection,]
    AME[i] = computeAME(model, data.interval, feature)[, feature]
    y.hat.mean[i] = mean(y.hat[selection])
    x.interval.average[i] = mean(x[selection])
  }
  bounds.rounded = round(bounds, digits = 3)
  interval.desc = character(l)
  interval.desc[l] = paste0("[", bounds.rounded[l-1], ", ", bounds.rounded[l], "]")
  for (i in 1:(l-1)) {
    interval.desc[i] = paste0("[", bounds.rounded[i], ", ", bounds.rounded[i+1], ")")
  }
  return(list(AME = setNames(AME, interval.desc), bounds = bounds, breaks = breaks,
    y.hat.mean = y.hat.mean, x.interval.average = x.interval.average, y.hat = y.hat, x = x))
}
