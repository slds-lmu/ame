library(readstata13)
library(margins)
# try to reproduce results form https://www.stata.com/manuals13/rmargins.pdf
data = read.dta13("tests/testthat/margex.dta")
data$outcome = as.factor(data$outcome)
data$group = as.factor(data$group)
data$treatment = as.factor(data$treatment)

checkEqualToMargins = function(margins, ame) {
  m = summary(margins)
  a = summary(ame)
  expect_equal(sort(unname(m$AME)), sort(unname(a$AME)))
}

###
f = outcome ~ treatment*group + age + I(age^2) + treatment:age
mod = glm(f, data = data, family = binomial)
summary(mod)

at1 = list(age = c(20,30,40,50,60))
mar1 = margins(mod, data = data, at = at1)
ame1 = computeAME(mod, data = data, features = c("treatment", "age", "group"), at = at1,
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
checkEqualToMargins(mar1, ame1)
#sort(extractSubList(lapply(unlist(ame1, recursive = FALSE), unlist), "age"))
#sort(mar1[names(mar1) == "age"])

at2 = list(group = c("1", "2"))
mar2 = margins(mod, data = data, at = at2)
ame2 = computeAME(mod, data = data, features = c("treatment", "age", "group"), at = at2,
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
#lapply(unlist(ame2, recursive = FALSE), unlist)
ame2
#sort(extractSubList(lapply(unlist(ame2, recursive = FALSE), unlist), "age"))
#sort(mar2[names(mar2) == "age"])

at3 = list(age = c(20, 30, 40), group = c("1", "2", "3"))
mar3 = margins(mod, data = data, at = at3)
ame3 = computeAME(mod, data = data, features = c("treatment", "age", "group"), at = at3,
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
ame3
#lapply(unlist(ame3, recursive = FALSE), unlist)
#sort(extractSubList(lapply(unlist(ame3, recursive = FALSE), unlist), "age"))
#sort(mar3[names(mar3) == "age"])

ame = computeAME(mod, data = data, features = c("treatment", "age", "group"),
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
sort(unlist(ame))


library(mmpf)
mp = marginalPrediction(data = data, vars = "age", n = c(nrow(data), nrow(data)), model = mod, uniform = FALSE,
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
plot(mp$age, mp$preds)

ame = computeAMEat(mod, data = data, features = c("age"), at = list(age = c(20,30,40,50,60)),
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
ame = as.data.frame(lapply(ame, function(x) {
  #d = data.frame(at.values = names(x), effect = unlist(x))
  lapply(x, function(i) (unlist(i)))
  #setDT(transpose(lapply(x, function(i) (unlist(i)))))
}))
points(x = 2:6*10, a["age",], col = "red")

mod = caret::train(f, data = data, family = "binomial", method = "glm")

load_all("../mlr")

lrn = makeLearner("classif.logreg", predict.type = "prob")
task = makeClassifTask(data = data, target = "outcome", positive = "1")
task$formula = f

mlr.mod = train(lrn, task)

unlist(computeAME(mlr.mod, data = data, features = c("treatment", "age", "group"))



bench = microbenchmark::microbenchmark(
  ame.simple = computeAME(mod, data = data, features = c("treatment", "age", "group"),
    method = "simple",
    predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response")),
  ame.richardson = computeAME(mod, data = data, features = c("treatment", "age", "group"),
    method = "Richardson",
    predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response")),
  ame.mlr = computeAME(mlr.mod, data = data, features = c("treatment", "age", "group"),
    predict.fun = function(object, newdata)
      getPredictionProbabilities(predict(object, newdata = newdata))),
  ame.margins = margins(mod, data = data),
  times = 200)
bench
