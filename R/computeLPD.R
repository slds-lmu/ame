#' Compute Local Partial Dependence
#'
#' Modification of Partial Dependence that only uses points nearby
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
computeLPD = function(model, data, feature, n, l = 6, predict.fun = predict, multiclass = FALSE) {
  x = data[, feature]
  x.grid = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n)

  if (multiclass) {
    y.hat.1 = predict.fun(model, newdata = data[1,])
    if(is.matrix(y.hat.1) | is.data.frame(y.hat.1)) {
      y.hat = matrix(nrow = n, ncol = ncol(y.hat.1))
      colnames(y.hat) = colnames(y.hat.1)
    } else {
      y.hat = matrix(nrow = n, ncol = length(y.hat.1))
      colnames(y.hat) = names(y.hat)
    }
  } else y.hat = numeric(n)

  for (i in 1:(n)) {
    distances = abs(x - x.grid[i])
    max.local.distance = sort(distances)[l]
    local.indices = which(distances <= max.local.distance)
    local.data = data[local.indices, ]
    local.data[, feature] = x.grid[i]
    y.hat[i,] = colMeans(predict.fun(model, newdata = local.data))
  }

  if (multiclass) {
  lpd.plot.data = reshape2::melt(data = data.frame(x = x.grid, y.hat), id.vars = "x", variable.name = "class", value.name = "probability")
  }
  return(list(y.hat = y.hat, x.grid = x.grid, n, l, plot.data = lpd.plot.data))
}
