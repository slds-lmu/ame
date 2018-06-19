context("check derivative")

test_that("check derivative", {
  feat = "treatment"
  predict.fun = function(object, newdata)
    predict(object, newdata = newdata, type = "response")

  deriv.factor = derivative(margex[[feat]], margex.mod, data = margex,
    feature = feat, predict.fun = predict.fun)

  margex[[feat]] = as.character(margex[[feat]])
  margex.mod = glm(margex.form, data = margex, family = binomial)
  deriv.character = derivative(margex[[feat]], margex.mod, data = margex,
    feature = feat, predict.fun = predict.fun)

  expect_equal(deriv.factor[["0"]], deriv.character[["0"]])
  expect_equal(deriv.factor[["1"]], deriv.character[["1"]])

  margex[[feat]] = margex[[feat]] == "1"
  margex.mod = glm(margex.form, data = margex, family = binomial)
  deriv.logical = derivative(margex[[feat]], margex.mod, data = margex,
    feature = feat, predict.fun = predict.fun)

  expect_equal(deriv.factor[["0"]], deriv.logical[["FALSE"]])
  expect_equal(deriv.factor[["1"]], deriv.logical[["TRUE"]])
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
