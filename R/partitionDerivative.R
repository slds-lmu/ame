#' Generate partitions with "similar" derivative
#'
#' @param x
#' @param y
#' @param max.splits
#'
#' @return Split points
#' @export
#'
#' @examples
partitionDerivative = function(x, y, max.splits) {
  mod = rpart::rpart(y ~ x, cp = 0, maxcompete = 0,
    minsplit = 1, minbucket = 1, xval = 0, maxsurrogate = 0)
  cp.ind = max(which(mod$cptable[,"nsplit"] <= max.splits))
  mod = rpart::prune(mod, cp = mod$cptable[cp.ind, "CP"])
  unname(mod$splits[,"index"])
}
