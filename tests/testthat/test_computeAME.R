context("check computeAME")

checkEqualToMargins = function(x, y) {
  expect_equal(sort(unname(summary(x)$AME)), sort(unname(summary(y)$AME)))
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

      predict.fun.glm = function(object, newdata) {
        pred = predict(object, newdata = newdata, type = "response")}
      predict.fun.caret = function(object, newdata) {
        predict(object, newdata = newdata, type = "prob")[["1"]]}
      ame = computeAME(mod, data = margex,
        features = c("treatment", "age", "group"), at = at,
        predict.fun = predict.fun.glm)
      ame.caret = computeAME(mod.caret, data = margex,
        features = c("treatment", "age", "group"), at = at,
        predict.fun = predict.fun.caret)
      checkEqualToMargins(ame, ame.caret)

      # FIXME: remove if-condition this when mlr tasks are able to use formula interface
      if (i == 1) {
        predict.fun.mlr = function(object, newdata) {
          predictions = predict(object, newdata = newdata)[["data"]]
          return(predictions[["prob.1"]])
        }
        ame.mlr = computeAME(
          mod.mlr, data = margex,
          features = c("treatment", "age", "group"), at = at,
          predict.fun = predict.fun.mlr)
        checkEqualToMargins(ame, ame.mlr)
      }
    }
  }
})
