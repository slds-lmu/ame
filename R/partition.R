#' Generate partitions with "similar" derivative
#'
#'
#' @param x
#' @param y
#' @param n.parts
#' @param part.method c("CART", "cluster")
#'
#' @section TODOs:
#' \itemize{
#'   \item Write tests.
#'   \item Alternative evaluation functions for cluster method.
#' }
#'
#' @return split.points numeric(length(n.parts)-1)
#' @export
#'
#' @examples
partition = function(x, y, n.parts, part.method="CART") {
  if (part.method == "CART") return(partitionCART(x, y, (n.parts-1)))
  #else if (part.method == "MOB") return(partitionMOB(x, y, n.parts))
  else if (part.method == "cluster") return(partitionCluster(x, y, n.parts))
  else stop("Partition method \'", part.method, "\' not implemented.")
}

#' Use mob
#partitionMOB = function(x, y, n.parts) {
  #TODO
#}

#' Use CART algorithm
partitionCART = function(x, y, max.splits) {
  mod = rpart::rpart(y ~ x, cp = 0, maxcompete = 0,
    minsplit = 1, minbucket = 1, xval = 0, maxsurrogate = 0)
  cp.ind = max(which(mod$cptable[,"nsplit"] <= max.splits))
  mod = rpart::prune(mod, cp = mod$cptable[cp.ind, "CP"])
  unname(mod$splits[,"index"])
}

#' Clustering-like method
#'
#' @param x
#' @param y
#' @param n.parts
#' @param eval.fun
partitionCluster = function(x, y, n.parts, eval.fun = absDiffMean) {
  assertNumeric(x)
  assertNumeric(y)
  assertIntegerish(n.parts)
  if (n.parts < 2) stop("\'n.pars\' has to be greater equal 2!")

  s = sort(x, index.return=TRUE)
  yp = as.list(y[s$ix])
  intervals = lapply(s$x, function(x) c(x, x))
  while(length(yp) > n.parts) {
    i = which.min(eval.fun(yp))
    ii = c(i, i+1)
    intervals[[i]] = c(intervals[[i]][1], intervals[[i+1]][2])
    intervals[[i+1]] = NULL
    yp[[i]] = unlist(yp[ii])
    yp[i+1] = NULL
  }
  split.points = vapply(seq.int(1, length(intervals)-1),
    function(i) mean(c(intervals[[i]][2], intervals[[i+1]][1])), FUN.VALUE = numeric(1))
  return(split.points)
}

#' Absolute values of differences of means of succeeding elements
#'
#' Calculate mean for every element of the list, calculate the differences between succeeding means,
#' and return absolute values of these differences.
#'
#' @param y list of numerical vectors
#'
#' @return numeric(length(y) - 1)
absDiffMean = function(y) {
  l = length(y)
  abs(vapply(1:(l-1), function(i) mean(y[[i+1]]) - mean(y[[i]]), FUN.VALUE = numeric(1)))
}
