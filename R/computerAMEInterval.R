computeAMEInterval = function(model, data, feature, breaks) {
  x = data[, feature]
  y.hat = predict(model, newdata = data)
  bounds = unique(c(min(x), sort(breaks), max(x) + 0.00001))
  l = length(bounds) - 1
  AME = numeric(l)
  y.hat.mean = numeric(l)
  x.median.interval = numeric(l)
  for (i in 1:l) {
    selection = x >= bounds[i] & x < bounds[i+1]
    data.interval = data[selection,]
    AME[i] = computeAME(model, data.interval, feature)[, feature]
    y.hat.mean[i] = mean(y.hat[selection])
    x.median.interval[i] = mean(x[selection])
  }
  bounds.rounded = round(bounds, digits = 3)
  interval.desc = character(l)
  interval.desc[l] = paste0("[", bounds.rounded[l-1], ", ", bounds.rounded[l], "]")
  for (i in 1:(l-1)) {
    interval.desc[i] = paste0("[", bounds.rounded[i], ", ", bounds.rounded[i+1], ")")
  }
  return(list(AME = setNames(AME, interval.desc), bounds = bounds,
    y.hat.mean = y.hat.mean, x.median.interval = x.median.interval, y.hat = y.hat, x = x))
}
