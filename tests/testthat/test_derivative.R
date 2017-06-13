library(readstata13)
library(margins)
data = read.dta13("tests/testthat/margex.dta")
data$outcome = as.factor(data$outcome)
data$group = as.factor(data$group)
data$treatment = as.factor(data$treatment)
f = outcome ~ treatment*group + age + I(age^2) + treatment:age

mean(derivative(data[["age"]], mod, data = data, feature = c("age"),
  predict.fun = function(object, newdata)
    predict(object, newdata = newdata, type = "response")))

sort(summary(margins(mod, data = data))$AME)

long = lapply(data[["age"]], function(x) {
  derivative(x, mod, data = data, feature = c("age"),
    predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
})


mean(derivative(mod, data = data, feature = c("age"),
  predict.fun = function(object, newdata)
    predict(object, newdata = newdata, type = "response")))
