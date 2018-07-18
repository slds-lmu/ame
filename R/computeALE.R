#' Compute Local Accumulated Effects (ALE)
#'
#' @section TODO:
#' \itemize{
#'   \item implement mlr models
#' }
#'
#' @param data a data.frame
#' @param model a model object
#' @param feature name(s) of the feature(s) of interest
#' @param K number of intervals
#'
#' @return
#' @export
#'
#' @examples
computeALE = function(model, data, feature, K, predict.fun = predict) {
  z = as.numeric(quantile(data[, feature], seq(0, 1, length.out = K+1), type = 1))
  z = unique(z) # if K > nrow(data) or data[, feature] has lots of non-unique values
  K = length(z) - 1
  interval.indices = as.numeric(cut(data[, feature], breaks = z, include.lowest = TRUE))
  data.l = data
  data.u = data
  data.l[, feature] = z[interval.indices]
  data.u[, feature] = z[interval.indices + 1]
  y.hat.l = predict.fun(object = model, newdata = data.l)
  y.hat.u = predict.fun(object = model, newdata = data.u)
  delta = y.hat.u - y.hat.l
  delta = as.numeric(tapply(delta, interval.indices, mean)) # provide alternative aggregation function
  fJ = c(0, cumsum(delta))
  b1 = as.numeric(table(a1))
  fJ = fJ - sum((fJ[1:K] + fJ[2:(K + 1)])/2 * b1)/sum(b1)
  x = z
}

#plot(x, fJ, type = "l", xlab = paste("x_", J, " (",
#  names(X)[J], ")", sep = ""), ylab = paste("f_",
#    J, "(x_", J, ")", sep = ""))

