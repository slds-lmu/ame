library(readstata13)
library(margins)
data = read.dta13("tests/testthat/margex.dta")
data$outcome = as.factor(data$outcome)
data$group = as.factor(data$group)
data$treatment = as.factor(data$treatment)

data = margex
head(data$outcome)
data$outcome = factor(data$outcome, levels = c("0", "1"))
head(data$outcome)

###
f = outcome ~ .#treatment*group + age + I(age^2) + treatment:age
mod = glm(f, data = data, family = binomial)
#summary(mod)
head(predict(mod, type = "response"))
head(data$outcome)

###
f = outcome ~ .
mod = glmboost(f, data = data, family = Binomial())
#summary(mod)
head(predict(mod, type = "response", newdata = data))
head(data$outcome)

at1 = list(age = c(20,30,40,50,60))
mar1 = margins(mod, data = data, at = at1)
ame1 = computeAME(mod, data = data, features = c("treatment", "age", "group"), at = at1,
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
ame1

at2 = list(group = c("1", "2"))
mar2 = margins(mod, data = data, at = at2)
ame2 = computeAME(mod, data = data, features = c("treatment", "age", "group"), at = at2,
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
ame2

at3 = list(age = c(20, 30, 40), group = c("1", "2", "3"))
mar3 = margins(mod, data = data, at = at3)
ame3 = computeAME(mod, data = data, features = c("treatment", "age", "group"), at = at3,
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
ame3
