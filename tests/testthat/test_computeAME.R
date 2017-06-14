context("check computeAME")

test_that("check computeAME", {
  checkEqualToMargins = function(x, y) {
    expect_equal(sort(unname(summary(x)$AME)), sort(unname(summary(y)$AME)))
  }

  # at wich points should the ame be computed
  at.list = list(
    list(age = c(20,30,40,50,60)),
    list(group = c("1", "2")),
    list(age = c(20, 30, 40), group = c("1", "2", "3")),
    NULL
  )

  # compute ame with marings package
  mar = lapply(at.list, function(at) {
    suppressWarnings(margins(margex.mod, data = margex, at = at))
  })

  # compute ame
  ame = lapply(at.list, function(at) {
    computeAME(margex.mod, data = margex, features = c("treatment", "age", "group"), at = at,
      predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
  })

  for (i in seq_along(at.list))
    checkEqualToMargins(mar[[i]], ame[[i]])
})

