library(readstata13)
library(margins)
# try to reproduce results form https://www.stata.com/manuals13/rmargins.pdf
load("data/margex.RData")
data = margex
data$outcome = as.factor(data$outcome)
data$group = as.factor(data$group)
data$treatment = as.factor(data$treatment)

form = outcome ~ treatment*group + age + I(age^2) + treatment:age
mod = glm(form, data = data, family = binomial)
mar = margins(mod, data = data)
summary(mar)

# bootrstap function
bootstrapSE = function(model.fun = function(formula, data) glm(formula, data = data, family = "binomial"),
  data, bs.iters = 30, features, at = NULL, predict.fun = NULL, cl = NULL, ...) {

  rdesc = mlr::makeResampleDesc("Bootstrap", iters = bs.iters)
  rin = mlr::makeResampleInstance(rdesc, size = nrow(data))
  ame = lapply(rin$train.inds, function(i) {
    model = model.fun(formula, data = data[i,])
    computeAME(model, data[i,], features = features, at = at,
      predict.fun = predict.fun, cl = cl, ...)
    })
}

# fit models on 100 bootstrapped data
bs1 = bootstrapSE(
  model.fun = function(formula, data) glm(formula = form, data = data, family = binomial),
  data = data,
  bs.iters = 100,
  features = c("age"),
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))

# sd of the AMEs is the standard error
sd(unlist(bs1))


