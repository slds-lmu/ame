#' Compute Partial Dependence
#'
#' Compute Partial Dependence with weighing or selecting observations based on distance.
#'
#' @section TODO:
#' \itemize{
#'   \item implement mlr models
#'   \item factor features?
#' }
#'
#' @template arg_model
#' @template arg_data
#' @param feature [\code{character(1)}]\cr
#'   Feature name, subset of \code{colnames(data)}.
#' @param n=nrow(data)/5 [\code{integer}]\cr Grid size
#' @param l=nrow(data) [\code{integer}]\cr Number of points that are defined as local to a grid point
#' @param wp=0 [\code{numeric}]\cr
#'   Defines the weights that are based on the distance from the grid point.
#'   Higher wp means more weight on nearby observations.
#'   wp=0: all weights equal 1 (standard partial dependence).
#' @template arg_predict.fun
#' @param multiclass=FALSE [\code{logical}]
#' @param derivative=FALSE [\code{logical}]
#'
#' @return
#' @export
#'
#' @examples
computePD = function(model, data, feature, n = "default", l = "default", wp = 0,
  predict.fun = function(object, newdata) predict(object, newdata = newdata),
  multiclass = FALSE, derivative = FALSE) {
  if (n == "default") n = nrow(data)/5
  if (l == "default") {
    l = nrow(data)
    lokal = FALSE
  } else lokal = TRUE
  assert_that(l <= nrow(data))

  x = data[, feature]
  x.grid = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n)

  if (multiclass) {
    y.hat.1 = predict.fun(model, newdata = data[1,])
    if (is.matrix(y.hat.1) | is.data.frame(y.hat.1)) {
      y.hat = matrix(nrow = n, ncol = ncol(y.hat.1))
      classes = colnames(y.hat.1)
      colnames(y.hat) = classes
    } else {
      y.hat = matrix(nrow = n, ncol = length(y.hat.1))
      classes = names(y.hat.1)
      colnames(y.hat) = classes
    }
  } else y.hat = numeric(n)

  for (i in 1:(n)) {
    distances = abs(x - x.grid[i])
    weights = (1 - normalize(distances))^wp
    if (lokal) {
      max.local.distance = sort(distances)[l]
      local.indices = which(distances <= max.local.distance)
      tmp.data = data[local.indices, ]
      tmp.data[, feature] = x.grid[i]
      weights = weights[local.indices]
    } else {
      tmp.data = data
      tmp.data[, feature] = x.grid[i]
    }
    if (derivative) {
      if (multiclass) {
        for (class in classes) {
          y.hat[i, class] = weighted.mean(derivative(tmp.data[, feature], feature, tmp.data, model,
            predict.fun = function(object, newdata) as.numeric(predict(object, newdata)[, class])), weights)
        }
      } else {
        y.hat[i] = weighted.mean(derivative(tmp.data[, feature], feature, tmp.data, model,
          predict.fun = predict.fun), weights)
      }
    } else {
      if (multiclass) y.hat[i,] = apply(predict.fun(model, newdata = tmp.data), 2, weighted.mean, weights)
      else y.hat[i] = weighted.mean(predict.fun(model, newdata = tmp.data), weights)
    }
  }

  if (multiclass) {
    plot.data = reshape2::melt(data = data.frame(x = x.grid, y.hat), id.vars = "x", variable.name = "class", value.name = "probability")
  } else {
    plot.data = data.frame(x = x.grid, y.hat)
  }
  return(list(y.hat = y.hat, x.grid = x.grid, plot.data = plot.data, n = n, l = l,
    multiclass = multiclass, feature = feature))
}

#' Plot partial dependence
#'
#' @param PD object created by \code{\link{computePD}}
#'
#' @return \code{ggplot}
#' @export
plotPD = function(PD) {
  if (PD$multiclass) {
    ggplot2::ggplot(data = PD$plot.data, aes(x = x, y = probability, group = class, col = class)) +
      ggplot2::geom_line() + ggplot2::geom_point() +
      xlab(PD$feature)
  } else {
    ggplot2::ggplot(data = PD$plot.data, mapping = aes(x, y.hat)) +
      ggplot2::geom_line() + ggplot2::geom_point() +
      xlab(PD$feature)
  }
}
