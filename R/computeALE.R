#' Compute Local Accumulated Effects (ALE)
#'
#' Implementation of \href{https://arxiv.org/abs/1612.08468}{Apley (2016) Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models}
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
#' @template arg_model
#' @template arg_data
#' @param feature [\code{character(1)}]\cr
#'   Feature name, subset of \code{colnames(data)}.
#' @param K number of intervals
#' @template arg_predict.fun
#' @param multiclass=FALSE
#' @param minbucket Not yet implemented.
#'
#' @return
#' @export
#'
#' @examples
computeALE = function(model, data, feature, K = "default", predict.fun = predict, multiclass = FALSE, minbucket = 1) {
  if (K == "default") K = nrow(data)/10
  x = data[, feature]
  if (K >= length(x)-1) {
    z = sort(x)
  } else {
    #z = as.numeric(quantile(x, seq(0, 1, length.out = K+1), type = 1))
    z = c(min(x), as.numeric(quantile(x, seq(1/K, 1, length.out = K),
      type = 1))) # c(min(), quantile()) necessary for K = n
  }
  z = unique(z) # if K > nrow(data) or x has lots of non-unique values
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
  #delta = y.hat.u - y.hat.l

  if (multiclass) { # multi-class
    nclass = ncol(y.hat.l)
    for (i in 1:nclass) {
      #delta[1:K,i] = tapply(delta[,i], interval.indices, mean)
      y.hat.l[1:K, i] = tapply(y.hat.l[,i], interval.indices, mean)
      y.hat.u[1:K, i] = tapply(y.hat.u[,i], interval.indices, mean)
    }
    y.hat.l = y.hat.l[1:K,]
    y.hat.u = y.hat.u[1:K,]
    delta = y.hat.u - y.hat.l
    #delta = delta[1:K, ]
    #f = apply(rbind(y.hat.l[1, ], delta), 2, function(x) cumsum(x))
    f = apply(delta, 2, function(x) c(0, cumsum(x)))
    f = f + matrix(y.hat.l[1,], K+1, nclass, byrow = TRUE)
    #f = apply(f, 2, function(f) f - sum((f[1:K] + f[2:(K + 1)])/2 * w) / sum(w))
    ale = apply(delta, 2, function(x) x/diff(z))
    ale.plot.data = reshape2::melt(data = data.frame(x = z, f), id.vars = "x", variable.name = "class", value.name = "probability")
    #ale.plot.data = reshape2::melt(data = data.frame(x = z[-length(z)], y.hat.l), id.vars = "x", variable.name = "class", value.name = "probability")
  } else {
    delta = y.hat.u - y.hat.l # probably better (numerically) to do tapply on y.hat, see multiclass
    delta = as.numeric(tapply(delta, interval.indices, mean))
    f = c(0, cumsum(delta))
    f = f - sum((f[1:K] + f[2:(K + 1)])/2 * w) / sum(w)
    ale = delta/diff(z)
    ale.plot.data = data.frame(x = z, f)
  }
  ale.x = z[-length(z)]
  return(list(x = z, f = f, K = K, i = interval.indices, ale = ale, ale.x = ale.x, ale.plot.data = ale.plot.data,
    feature = feature))
}

#plot(x, fJ, type = "l", xlab = paste("x_", J, " (",
#  names(X)[J], ")", sep = ""), ylab = paste("f_",
#    J, "(x_", J, ")", sep = ""))
plotALE = function(ALE) {
  ggplot(data = ALE$ale.plot.data, aes(x = x, y = f)) + geom_line() + geom_point() +
    xlab(ALE$feature)
}
