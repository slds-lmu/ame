#' Compute marginal average effects
#'
#' @param object Output object of training a model or WrappedModel (mlr package)
#' @param data data.frame
#' @param features character Name(s) of the feature(s) AME should be computed for.
#' @param predict.fun Custom prediction function
#' @param delta numeric Delta for computing AME of numeric features. Default is .0001.
#' @param parallel logical (default is FALSE) If true, computation of several features is done with
#'   mclapply(). Does not work on Windows systems.
#'
#' @section Categorical features:
#'
#' Supported classes are \code{factor}, \code{logical}, \code{character}. First level is chosen as reference category.
#'
#'
#' @section Custom prediction function:
#'
#' If you are using mlr you do not have to provide a prediciton function.
#'
#' If you are using a model from an arbitrary R package, you have to make sure that the predict method
#' is in the form \code{predict(object, newdata = newdata)} and returns a numeric value. Furthermore:
#'
#' Regression tasks: provide predict.fun if the model needs specific arguments,
#'   e.g. gbm needs the argument \code{n.trees}.
#'
#' Classification tasks: you have to make sure that the prediction function of the model returns
#'   probabilities, e.g.:
#' \itemize{
#'   \item glm: predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response")
#'   \item gbm: predict.fun = function(object, newdata) predict(object, newdata = newdata, n.trees = 1000, type = "response")
#' }
#'
#' @return AME
compAME = function(object, ...) {
  UseMethod("compAME", object = object)
}

compAME.default = function(object, data, features, predict.fun = NULL, individual = FALSE,
    delta = NULL, parallel = FALSE, ...) {
  assertDataFrame(data)
  assertSubset(features, colnames(data))
  assertFunction(predict.fun, args = c("object", "newdata"), null.ok = TRUE)
  assertNumeric(delta, null.ok = TRUE)
  assertLogical(parallel)
  if (is.null(delta)) delta = .0001 # default value by which a feature value is shifted
  #else if (delta == "trivedi") delta = sd(data[,features[1]]) / 1000 # suggested by Cameron/Trivedi 2009
  #else assertNumeric(delta)
  if(is.null(predict.fun)) predict.fun = function(object, data) predict(object, newdata = data)

  if (individual) aggr.fun = function(x) x
  else aggr.fun = mean

  if (parallel) {
    mc.cores = parallel::detectCores()
    ame = parallel::mclapply(features, function(feature) {
      x = data[[feature]]
      compAMEFeature(x, object, feature, data, predict.fun, aggr.fun, delta)
    }, mc.cores = mc.cores)
  } else {
    ame = lapply(features, function(feature) {
      x = data[[feature]]
      compAMEFeature(x, object, feature, data, predict.fun, aggr.fun, delta)
    })
  }
  names(ame) = features
  return(ame)
}

#' WrappedModel class from mlr package
compAME.WrappedModel = function(object, task, features, delta = NULL, parallel = FALSE,...) {
  assertClass(task, classes = "Task")
  data = mlr::getTaskData(task)
  task.type = mlr::getTaskType(task)
  if (task.type == "regr") {
    predict.fun = function(object, newdata) {
      mlr::getPredictionResponse(predict(object, newdata = newdata))
    }
  } else if (task.type == "classif") {
    predict.fun = function(object, newdata) {
      mlr::getPredictionProbabilities(predict(object, newdata = newdata))
    }
  } else stop("Task type of ", deparse(substitute(task)), " is not supported.")

  compAME.default(object = object, data = data, features = features, predict.fun = predict.fun,
    delta = delta, parallel = parallel)
}
