library(readstata13)
library(margins)
load("data/margex.RData")
data = margex
str(data)

### tree-split function
splitPD = function(x, y, max.splits) {
  # FIXME: How to solve http://stackoverflow.com/questions/27862280/tree-sizes-given-by-cp-table-in-rpart
  mod = rpart::rpart(y ~ x, cp = 0, maxcompete = 0,
    minsplit = 1, minbucket = 1, xval = 0, maxsurrogate = 0)
  cp.ind = max(which(mod$cptable[,"nsplit"] <= max.splits))
  mod = rpart::prune(mod, cp = mod$cptable[cp.ind, "CP"])
  unname(mod$splits[,"index"])
}

# fit model
data$distance = log(data$distance)
f = outcome ~ distance + ycn + I(distance^2) + age + I(age^2)
mod = glm(f, data = data, family = binomial)

# ame of distance
computeAME(mod, data, "distance")

# compute the derivative of distance
yderiv = derivative(data$distance, "distance", data, mod)
mean(yderiv) # average of this is the ame

# for each value of distance predict the average y-value
library(BBmisc)
y = vnapply(data$distance, function(x) predictModifiedData(x, "distance", data, mod,
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response")))

# plot averaged effect of distance
plot(data$distance, y)
abline(v = splitPD(data$distance, yderiv, 3))
