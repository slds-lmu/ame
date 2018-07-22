#' Compute Local Accumulated Effects (ALE)
#'
#' @section TODO:
#' \itemize{
#'   \item implement mlr models
#'   \item factor features
#'   \item minbucket
#'   \item pass specific interval boundaries (z)
#'   \item support multi-class (work-around: let predict function return probs of one class)
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
computeALE = function(model, data, feature, K, predict.fun = predict, multiclass = FALSE, minbucket = 5) {
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
  w = as.numeric(table(interval.indices))

  data.l = data
  data.u = data
  data.l[, feature] = z[interval.indices]
  data.u[, feature] = z[interval.indices + 1]
  y.hat.l = predict.fun(model, newdata = data.l)
  y.hat.u = predict.fun(model, newdata = data.u)
  delta = y.hat.u - y.hat.l

  if (multiclass) { # multi-class
    nclass = ncol(delta)
    for (i in 1:nclass) {
      delta[1:K,i] = tapply(delta[,i], interval.indices, mean)
    }
    delta = delta[1:K, ]
    f = apply(delta, 2, function(x) c(0, cumsum(x)))
    f = apply(f, 2, function(f) f - sum((f[1:K] + f[2:(K + 1)])/2 * w) / sum(w))
    #f = f + matrix(y.hat.l[1,], K+1, nclass, byrow = TRUE)
    ale = apply(delta, 2, function(x) x/diff(z))
    ale.plot.data = reshape2::melt(data = data.frame(x = z, f), id.vars = "x",
      variable.name = "class", value.name = "probability")
  } else {
    delta = as.numeric(tapply(delta, interval.indices, mean)) # provide alternative aggregation function
    f = c(0, cumsum(delta))
    f = f - sum((f[1:K] + f[2:(K + 1)])/2 * w) / sum(w)
    ale = delta/diff(z)
    ale.plot.data = data.frame(x = z, f)
  }
  return(list(x = z, f = f, K = K, i = interval.indices, ale = ale, ale.plot.data = ale.plot.data))
}

#plot(x, fJ, type = "l", xlab = paste("x_", J, " (",
#  names(X)[J], ")", sep = ""), ylab = paste("f_",
#    J, "(x_", J, ")", sep = ""))

