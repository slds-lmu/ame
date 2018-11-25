context("check derivative")

test_that("check derivative", {
  all.features = colnames(margex)[!colnames(margex) %in% "treatment"]
  derivatives = lapply(all.features, FUN = function(feature) {
    predict.fun = function(object, newdata)
      predict(object, newdata = newdata, type = "response")
    deriv = derivative(margex[[feature]], margex.mod, data = margex,
                              feature = feature, predict.fun = predict.fun)
    expect_error(expect_error(derivatives))
  })
})
  # margex[[feat]] = as.character(margex[[feat]])
  # margex.mod = glm(margex.form, data = margex, family = binomial)
  # deriv.character = derivative(margex[[feat]], margex.mod, data = margex,
  #   feature = feat, predict.fun = predict.fun)

  # expect_equal(deriv.factor, deriv.character)
  #
  # deleted tests for character features because function now only accepts
  # numeric and factor features
  # margex[[feat]] = margex[[feat]] == "1"
  # margex.mod = glm(margex.form, data = margex, family = binomial)

    # deriv.logical = derivative(margex[[feat]], margex.mod, data = margex,
  #   feature = feat, predict.fun = predict.fun)
  #
  # names(deriv.logical) = as.numeric(as.logical(names(deriv.logical)))
  # expect_equal(deriv.factor, deriv.logical[names(deriv.factor)])

  #
  # deleted tests for logical features because function now only accepts
  # numeric and factor features

#
# features = c("age")
#
#
# margins.ame = suppressWarnings(summary(margins::margins(margex.mod, data = margex))$AME)
#
# for (feat in features) {
#   mean(derivative(margex[[feat]], margex.mod, data = margex, feature = feat,
#     predict.fun = function(object, newdata)
#       predict(object, newdata = newdata, type = "response")))
#
# }
# long = lapply(margex[["age"]], function(x) {
#   derivative(x, margex.mod, data = margex, feature = c("age"),
#     predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
# })
