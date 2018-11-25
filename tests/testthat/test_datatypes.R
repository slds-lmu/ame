context("check datatypes")

test_that("check computeAME for rejection of unallowed data types", {
  lrn = mlr::makeLearner("classif.svm", predict.type = "prob")
  mod = mlr::train(task = margex.task, learner = lrn)
  computeAME(data = margex, model = mod, features = "sex")

  margex.modified = margex
  class(margex.modified$sex) = "character"

  expect_error(
    computeAME(data = margex.modified, model = mod, features = "sex"))

  margex.modified = margex
  class(margex.modified$treatment) = "logical"

  expect_error(
    computeAME(data = margex.modified, model = mod, features = "sex"))

})
