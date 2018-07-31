context("check derivative")

test_that("glm model", {
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

test_that("data with numeric, factor, logic and character", {
  ################
  # generate data
  set.seed(4218)
  n = 100
  bin1 = rbinom(n, 1, prob = .75)
  cat1.map = setNames(c(0,-2,2,6), c("blue", "red2", "red", "green"))
  cat1 = sample(factor(cat1.map, labels = names(cat1.map)), n, replace = TRUE, prob = c(.25, .25, .25, .25))
  ord1.map = setNames(c(0,4,5,10,20), c("low", "midlow", "mid", "midhi", "high"))
  ord1 = sample(factor(ord1.map, labels = names(ord1.map)), n, replace = TRUE, prob = c(.1, .2, .2, .3, .2))
  logic1 = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(.45, .55))
  char1 = sample(c("A", "E", "I"), n, replace = TRUE, prob = c(.3, .4, .3))
  x1 = runif(n, -.5, .5)
  eps = rnorm(n, 0, 2)
  # data generating process:
  y = .4 + 2.2 * x1 + 2.3 * bin1 + .1 * cat1.map[cat1] + .1 * ord1.map[ord1] + logic1 * 1.3 +
    ifelse(char1 == "A", 0, ifelse(char1 == "E", -1, 1)) + eps
  df = data.frame(y, x1, bin1 = factor(bin1), cat1, ord1, char1, logic1, stringsAsFactors = FALSE)
  ##########################

  # linear model coefficients should be equal to AME
  lm.mod = lm(y ~ ., data = df)
  lm.coef = coef(lm.mod)
  lm.ame = computeAME(lm.mod, df, names(df)[-1])
  expect_equal(unlist(lm.ame), lm.coef[-1])

  # random forest
  df[["char1"]] = NULL # random forest does not allow characters
  rf.mod = randomForest::randomForest(y ~ ., data = df, ntree = 500, se.ntree = 100, se.method = "sd",
    se.boot = 50, replace = TRUE, nodesize = 5, importance = FALSE, localImp = FALSE, nPerm = 1,
    proximity = FALSE, do.trace = FALSE, keep.forest = TRUE, keep.inbag = FALSE)
  expect_length(unlist(computeAME(rf.mod, df, names(df)[-1])), 10)
})
