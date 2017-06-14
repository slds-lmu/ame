context("check derivative")

test_that("check derivative", {
  feat = "treatment"

  deriv.factor = derivative(margex[[feat]], margex.mod, data = margex, feature = feat,
    predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))

  margex[[feat]] = as.character(margex[[feat]])
  margex.mod = glm(margex.form, data = margex, family = binomial)
  deriv.character = derivative(margex[[feat]], margex.mod, data = margex, feature = feat,
    predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))

  expect_equal(deriv.factor, deriv.character)

  margex[[feat]] = margex[[feat]] == "1"
  margex.mod = glm(margex.form, data = margex, family = binomial)
  deriv.logical = derivative(margex[[feat]], margex.mod, data = margex, feature = feat,
    predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))

  names(deriv.logical) = as.numeric(as.logical(names(deriv.logical)))
  expect_equal(deriv.factor, deriv.logical[names(deriv.factor)])
})
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
