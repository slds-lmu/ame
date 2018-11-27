#' @param predict.fun [\code{function(1)}]\cr
#'   The function that should be used to generate predictions from \code{model}.
#'   The specified prediction function is subject to subsequent numeric
#'   differentiation and thus needs to be carefully chosen in order to receive the correct
#'   results. This function must have two arguments, \code{object} and \code{newdata}.
#'   The default is the predict method for \code{model}.
#'   If \code{model} is of class \code{\link[mlr]{WrappedModel}}, the default tries to use
#'   \code{\link[mlr]{getPredictionProbabilities}} or \code{\link[mlr]{getPredictionResponse}}
#'   depending on whether \code{model} is a classification or regression problem.
