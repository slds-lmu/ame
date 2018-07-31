#' Compute Average Marginal Effects
#'
#' Computes the average marginal effects for specified features.
#'
#' @section Categorical features:
#'
#' Supported classes are \code{factor}, \code{logical}, \code{character} (also depends on the model function).
#' First level is chosen as reference category.
#'
#' @section Custom prediction function:
#'
#' If you are using mlr model objects you do not have to provide a prediciton function.
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
#'   \item gbm: predict.fun = function(object, newdata) predict(object, newdata = newdata,
#'   n.trees = 1000, type = "response")
#' }
#'
#' @template arg_model
#' @template arg_data
#' @param features [\code{character}]\cr
#'   The features for which the average marginal effects should be computed.
#' @param at [\code{list}]\cr
#'   (optional) A named list of vectors where the values specify at which points the marginal effects are calculated (i.e. the values are held constant).
#' @template arg_predict.fun
#' @param ... Passes arguments on to \link[numDeriv]{grad}.
#'   Further options passed down to the \code{\link[numDeriv]{grad}} function.
#'
#' @export
computeAME = function(model, data, features, at = NULL, predict.fun = NULL, cl = NULL, ...) {
  assertDataFrame(data)
  assertSubset(features, colnames(data))
  assertList(at, types = "vector", null.ok = TRUE)
  assertFunction(predict.fun, args = c("object", "newdata"), null.ok = TRUE)

  if (is.null(at)) {
    ame = computeAMEInternal(model, data, features, predict.fun, cl = cl, ...)
    ret = as.data.frame(as.list(unlist(ame)))
  } else {
    assertNames(names(at), subset.of = colnames(data))

    # create grid from all combinations in 'at'
    grid = expand.grid(filterNull(at[colnames(data)]))

    # replace the feature values in the data.frame with the constant values in grid and compute the ame
    ind.cols = which(colnames(data) %in% colnames(grid))
    ret = lapply(seq_row(grid), function(i) {
      gr = grid[i, , drop = FALSE]
      d = replace(data, list = ind.cols, values = gr)
      ame = computeAMEInternal(model, d, features, predict.fun, ...)
      as.data.frame(as.list(unlist(ame)))
    })

    # at.vars = names(at)
    # iterate over features used in at.vars
    # ret = lapply(at.vars, function(vars) {
    #   # iterate over the values defined in 'at'
    #   ame = lapply(at[[vars]], function(vals) {
    #     d = replace(data, list = which(colnames(data) == vars) , vals)
    #     computeAMEInternal(model, d, features, predict.fun, ...)
    #   })
    #   setNames(ame, at[[vars]])
    # })
    # ret = setNames(ret, at.vars)
    colnames(grid) = sprintf("at(%s)", colnames(grid))
    ret = cbind(grid, rbindlist(ret, fill = TRUE))
  }
  return(addClasses(ret, "AME"))
}

# print.AME = function(x, ...) {
#   print(as.data.frame(lapply(x, function(x) {
#     #d = data.frame(at.values = names(x), effect = unlist(x))
#     lapply(x, function(i) (unlist(i)))
#     #setDT(transpose(lapply(x, function(i) (unlist(i)))))
#   })))
# }

summary.AME = function(x, ...) {
  id.vars = colnames(x)[grepl("^at\\(.*\\)$", colnames(x))]
  ret = melt(x, id.vars = id.vars, variable.name = "factor", value.name = "AME")
  colnames(ret) = gsub("^at\\(|\\)$", "", colnames(ret))
  return(ret)
}
