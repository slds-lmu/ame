context("check computeAME")

checkEqualToMargins = function(x, y) {
  # expect_equal(sort(unname(summary(x)$AME)), sort(unname(summary(y)$AME)))
  # only compare absolute values as long you can't choose "positive class"
  expect_equal(sort(abs(unname(summary(x)$AME))), sort(abs(unname(summary(y)$AME))))
}

test_that("check computeAME for classification", {
  task = mlr::subsetTask(margex.task, features = c("age", "treatment"))
  lrn = mlr::makeLearner("classif.logreg", predict.type = "prob")

  at.list = list(
    list(age = c(20,30,40,50,60)),
    list(group = c("1", "2")),
    list(age = c(20, 30, 40), group = c("1", "2", "3")),
    NULL
  )

  form = list(
    form1 = outcome ~ age + treatment,
    form2 = outcome ~ treatment*group + age + I(age^2) + treatment:age
  )

  for (i in seq_along(form)) {
    mod = glm(form[[i]], data = margex, family = binomial)
    mod.caret = caret::train(form[[i]], data = margex, method = "glm")
    mod.mlr = mlr::train(lrn, task)

    for (at in at.list) {
      predict.fun = function(object, newdata)
        predict(object, newdata = newdata, type = "response")
      ame = computeAME(mod, data = margex, cl = "1",
        features = c("treatment", "age", "group"), at = at, predict.fun = predict.fun)
      ame.caret = computeAME(mod.caret, data = margex, cl = "1",
        features = c("treatment", "age", "group"), at = at)
      checkEqualToMargins(ame, ame.caret)

      # FIXME: remove if-condition this when mlr tasks are able to use formula interface
      if (i == 1) {
        ame.mlr = computeAME(mod.mlr, data = margex, cl = "1",
          features = c("treatment", "age", "group"), at = at)
        checkEqualToMargins(ame, ame.mlr)
      }
    }
  }
})
