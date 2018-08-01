#' Plot AME
#'
#' @param object Model object
#' @param data data.frame
#' @param ame AME object
#' @param target character(1) Name of target variable.
#'
#' @section TODOS:
#' \itemize{
#' \item Wrapped.Model / task support
#' \item classification support
#' \item Facet of different models (i.e. draw multiple AME lines for one feature)
#' }
#'
#' @export
plotAME = function(object, data, ame, target) {
  features = names(ame)
  data = data[, c(features, target)]
  target.mean = mean(data[, target])
  features.mean = colMeans(data[, features, drop = FALSE])
  intercepts = mean(data[, target]) - ame * features.mean

  data.plot = reshape2::melt(data, id.vars = target, variable.name = "feature", value.name = "x")
  data.plot[["y.ame"]] = numeric(nrow(data.plot))
  for (feature in features) {
    data.plot[data.plot$feature==feature, "y.ame"] =
      intercepts[feature] + data.plot[data.plot$feature == feature, "x"] * ame[feature]
  }
  #ggplot() +
  #  geom_point(data = data, mapping = aes_(x = as.name(features), y = as.name(target)), alpha = .3) +
  #  geom_abline(mapping = aes(slope = ame, intercept = intercept, color = color.group), show.legend = show.legend)
  ggplot2::ggplot(data = data.plot, mapping = aes(x = x)) +
    ggplot2::geom_point(mapping = aes(y = y), alpha = .3) +
    ggplot2::geom_line(mapping = aes(y = y.ame), col = "red") +
    ggplot2::facet_wrap(~ feature, scales = "free_x")
}

#' Plot AME intervals
#'
#' @export
plotAMEInterval = function(AMEInterval) {
  AME = AMEInterval$AME
  x.0 = AMEInterval$x.interval.average
  y.0 = AMEInterval$y.hat.mean
  bounds = AMEInterval$bounds
  p = ggplot() +
    geom_point(mapping = aes(AMEInterval$x, AMEInterval$y.hat), pch = 16, alpha = .2)
  for(i in 1:(length(bounds)-1)) {
    p = p + geom_line(mapping = aes_string(bounds[i:(i+1)], c(y.0[i] - (x.0[i] - bounds[i]) * AME[i],
      y.0[i] + (bounds[i+1] - x.0[i]) * AME[i])), col = "green", inherit.aes = FALSE)
  }
  return(p)
}

