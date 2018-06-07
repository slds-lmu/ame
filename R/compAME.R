#' Compute marginal average effects
#'
#' @param object Model object
#' @param x Feature
#' @param delta Delta for computing AME
#'
#' @return AME
#'
#' @examples 4
#'
#' @export
compAME = function(object, ...) {
  UseMethod("compAME", object = object)
}

#' @export
compAME.default = function(object, data, features, predict.fun = NULL, delta = NULL, parallel = FALSE, ...) {
  assertDataFrame(data)
  assertSubset(features, colnames(data))
  assertFunction(predict.fun, args = c("object", "newdata"), null.ok = TRUE)
  assertNumeric(delta, null.ok = TRUE)
  assertLogical(parallel)

  compute1Feature = function(feature, delta) {
    x = data[[feature]]
    data.delta = data
    if(is.numeric(x)) {
      data.delta[, feature] = data.delta[, feature] + delta
      prediction1 = predict.fun(object, data)
      prediction2 = predict.fun(object, data.delta)
      ame = mean((prediction2 - prediction1) / delta)
    } else if(is.factor(x)) {
      # only supports binary 0/1 coding
      lvls = levels(x)
      data[, feature] = factor(lvls[1], levels = lvls)
      data.delta[, feature] = factor(lvls[2], levels = lvls)
      prediction1 = predict.fun(object, data)
      prediction2 = predict.fun(object, data.delta)
      ame = mean(prediction2 - prediction1) # support alternative aggr functions
    } else stop("Data type of ", feature, " is unsupported.")
    return(ame)
  }

  if (is.null(delta)) delta = .0001 # default value by which a feature value is shifted
  #else if (delta == "trivedi") delta = sd(data[,features[1]]) / 1000 # suggested by Cameron/Trivedi 2009
  if(is.null(predict.fun)) predict.fun = function(object, data) predict(object, newdata = data)

  if (parallel) {
    mc.cores = parallel::detectCores()
    ame = parallel::mclapply(features, function(x) compute1Feature(feature = x, delta = delta), mc.cores = mc.cores)
  } else {
    ame = lapply(features, function(x) compute1Feature(feature = x, delta = delta))
  }
  names(ame) = features
  return(unlist(ame))
}

#' WrappedModel class from mlr package
#'
#' @export
compAME.WrappedModel = function(object, task, features, predict.fun = NULL, delta = NULL, parallel = FALSE,...) {
  assertClass(task, classes = "Task")
  data = mlr::getTaskData(task)

  if(is.null(predict.fun)) predict.fun = function(object, newdata) {
    mlr::getPredictionResponse(predict(object, newdata = newdata))
  }

  compAME.default(object = object, data = data, features = features, predict.fun = predict.fun, delta = delta, parallel = parallel)
}
