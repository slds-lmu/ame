#' Compute Local Accumulated Effects (ALE)
#'
#' @section TODO:
#' \itemize{
#'   \item implement mlr models
#'   \item factor features
#'   \item minbucket
#'   \item pass specific interval boundaries (z)
#'   \item second-order effects
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
computeALE = function(model, data, feature, K, predict.fun = predict, minbucket = 5) {
  x = data[, feature]
  if (K >= length(x)-1) z = sort(x)
  else {
    #z = as.numeric(quantile(x, seq(0, 1, length.out = K+1), type = 1))
    z = c(min(x), as.numeric(quantile(x, seq(1/K, 1, length.out = K),
      type = 1))) # c(min(), quantile()) necessary for K = n
    z = unique(z) # if K > nrow(data) or x has lots of non-unique values
  }
  K = length(z) - 1
  # if K >= (n-1) the first two obs are assigned to the first interval
  interval.indices = as.numeric(cut(x, breaks = z, include.lowest = TRUE))
  data.l = data
  data.u = data
  data.l[, feature] = z[interval.indices]
  data.u[, feature] = z[interval.indices + 1]
  y.hat.l = predict.fun(object = model, newdata = data.l)
  y.hat.u = predict.fun(object = model, newdata = data.u)
  delta = y.hat.u - y.hat.l
  delta = as.numeric(tapply(delta, interval.indices, mean)) # provide alternative aggregation function
  #f = cumsum(c(mean(y.hat.l[interval.indices == 1]), delta))
  f = c(0, cumsum(delta))
  w = as.numeric(table(interval.indices))
  f = f - sum((f[1:K] + f[2:(K + 1)])/2 * w) / sum(w)
  return(list(x = z, f = f, K = K, i = interval.indices, ale = delta/diff(z)))
}

#plot(x, fJ, type = "l", xlab = paste("x_", J, " (",
#  names(X)[J], ")", sep = ""), ylab = paste("f_",
#    J, "(x_", J, ")", sep = ""))

