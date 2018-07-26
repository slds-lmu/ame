# computeLPD = function(model, ...) {
#   UseMethod("computeAMEInternal")
# }

#' Compute Weighted Partial Dependence
#'
#' Modification of Partial Dependence that only uses points nearby
#'
#' @section TODO:
#' \itemize{
#'   \item implement mlr models
#'   \item factor features?
#'   \item add derivative argument
#' }
#'
#' @param model Fitted model object
#' @param data (data.frame) Data used to fit the model
#' @param feature (character) Feature name, subset of data
#' @param n (integer) Grid size
#' @param l=6 (integer) Number of points that are defined as local to a grid point
#' @param predict.fun (function) Prediciton function
#' @param multiclass=FALSE (logical)
#'
#' @return
#' @export
#'
#' @examples
computeWPD = function(model, data, feature, n = "default", l = "default", wp = 4, predict.fun = predict,
  multiclass = FALSE, derivative = FALSE) {
  if (n == "default") n = nrow(data)/2
  if (l == "default") l = nrow(data)/10
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
      y.hat = as.data.frame(matrix(nrow = n, ncol = length(y.hat.1)))
      classes = names(y.hat.1)
      colnames(y.hat) = classes
    }
  } else y.hat = numeric(n)

  for (i in 1:(n)) {
    distances = abs(x - x.grid[i])
    weights = (1 - normalize(distances))^wp
    data[, feature] = x.grid[i]
    if (derivative) {
      if (multiclass) {
        for (class in classes) {
          y.hat[i, class] = weighted.mean(derivative(data[, feature], feature, data, model,
            predict.fun = function(object, newdata) as.numeric(predict(object, newdata)[, class])), weights)
        }
      } else {
        y.hat[i] = weighted.mean(derivative(data[, feature], feature, data, model,
          predict.fun = predict.fun), weights)
      }
    } else {
      if (multiclass) y.hat[i,] = apply(predict.fun(model, newdata = data), 2, weighted.mean, weights)
      else y.hat[i] = weighted.mean(predict.fun(model, newdata = data), weights)
    }
  }

  if (multiclass) {
    lpd.plot.data = reshape2::melt(data = data.frame(x = x.grid, y.hat), id.vars = "x", variable.name = "class", value.name = "probability")
  } else {
    lpd.plot.data = data.frame(x = x.grid, y.hat)
  }
  return(list(y.hat = y.hat, x.grid = x.grid, plot.data = lpd.plot.data, n = n, l = l,
    multiclass = multiclass, feature = feature))
}

# computeLPD.WrappedModel = function(model, task) {
#
# }

# plotLPD = function(LPD) {
#   if (LPD$multiclass) {
#     ggplot2::ggplot(data = LPD$plot.data, aes(x = x, y = probability, group = class, col = class)) +
#       ggplot2::geom_line() + ggplot2::geom_point() +
#       xlab(LPD$feature)
#   } else {
#     ggplot2::ggplot(data = LPD$plot.data, mapping = aes(x, y.hat)) +
#       ggplot2::geom_line() + ggplot2::geom_point() +
#       xlab(LPD$feature)
#   }
# }
