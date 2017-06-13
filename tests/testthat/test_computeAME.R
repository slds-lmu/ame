library(readstata13)
library(margins)
data = read.dta13("tests/testthat/margex.dta")
data$outcome = as.factor(data$outcome)
data$group = as.factor(data$group)
data$treatment = as.factor(data$treatment)

checkEqualToMargins = function(x, y) {
  expect_equal(sort(unname(summary(x)$AME)), sort(unname(summary(y)$AME)))
}

###
f = outcome ~ treatment*group + age + I(age^2) + treatment:age
mod = glm(f, data = data, family = binomial)
summary(mod)

at.list = list(
  list(age = c(20,30,40,50,60)),
  list(group = c("1", "2")),
  list(age = c(20, 30, 40), group = c("1", "2", "3")),
  NULL
)

mar = lapply(at.list, function(at) {
  margins(mod, data = data, at = at)
})

ame = lapply(at.list, function(at) {
  computeAME(mod, data = data, features = c("treatment", "age", "group"), at = at,
    predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
})

for (i in seq_along(at.list))
  checkEqualToMargins(mar[[i]], ame[[i]])

