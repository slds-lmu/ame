library(mlr)
library(mmpf)
library(ggplot2)
devtools::load_all()

####################-#
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + geom_point()
ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, col = Species)) + geom_point()

train.sub = rep(TRUE, getTaskSize(iris.task))
#train.sub[c(sample(1:50, 10), sample(1:50, 10) + 50, sample(1:50, 10) + 100)] = FALSE
nnet.lrn = makeLearner("classif.nnet", skip = FALSE, size = 20, decay = 0.0001, maxit = 1000,
  trace = FALSE, predict.type = "prob")
nnet.mod = train(nnet.lrn, iris.task, subset = train.sub)
#performance(predict(nnet.mod, iris.task, subset = !train.sub), measures = list(mmce, acc))

#########################-#
# compute LPD manually ####
n = 20 # number of grid points
l = 6 # number of local points
feature = "Petal.Width"
x = iris[, feature]
z = sort(x)
x.grid = uniformGrid(x, n)
x.grid[1]
distances = abs(x - x.grid[1])
max.local.distance = sort(distances)[l]
obs.ind = which(distances <= max.local.distance) # indices of all local points
tmp.data = iris[obs.ind, ]
# predict y.hat for all local points at grid point 1 and take the mean:
tmp.data[, feature] = x.grid[1]
y.hat = colMeans(predict(nnet.mod$learner.model, newdata = tmp.data))
y.hat
marginalPrediction(iris, feature, n = 1, model = nnet.mod$learner.model, int.points = obs.ind)

# 2nd grid point:
x.grid[2]
(distances = abs(x - x.grid[2]))
(max.local.distance = sort(distances)[l])
(obs.ind = which(distances <= max.local.distance))
tmp.data = iris[obs.ind, ]
tmp.data[, feature] = x.grid[2]
y.hat = colMeans(predict(nnet.mod$learner.model, newdata = tmp.data))
y.hat
marginalPrediction(iris, feature, n = n, model = nnet.mod$learner.model, int.points = obs.ind)[1:3]


####################-
# computeLPD:

# Petal.Width:
LPD.petwid = computePD(nnet.mod$learner.model, iris, "Petal.Width", n = 10, l = 15, multiclass = TRUE)
plotPD(LPD.petwid)
plotPartialDependence(generatePartialDependenceData(nnet.mod, iris.task, features = "Petal.Width", n = 20))

plotPD(computePD(nnet.mod$learner.model, iris, "Petal.Width", n = 20, wp = 4, multiclass = TRUE))

#derivative
plotPartialDependence(generatePartialDependenceData(nnet.mod, iris.task, features = "Petal.Width", n = 20,
  derivative = TRUE))
plotPD(computePD(nnet.mod$learner.model, iris, "Petal.Width", n = 200, l = 20, multiclass = TRUE, derivative = TRUE))
plotPD(computePD(nnet.mod$learner.model, iris, "Petal.Width", n = 200, wp = 4, multiclass = TRUE, derivative = TRUE))

# Petal.Length:
LPD.petlen = computePD(nnet.mod$learner.model, iris, "Petal.Length", n = 40, l = 15, multiclass = TRUE)
plotPD(LPD.petlen)
plotPartialDependence(generatePartialDependenceData(nnet.mod, iris.task, features = "Petal.Length", n = 40))

WPD.petlen = computePD(nnet.mod$learner.model, iris, "Petal.Length", n = 40, wp = 4, multiclass = TRUE)
plotPD(WPD.petlen)

########################-
# Gradient Boosting ####
gbm.lrn = makeLearner("classif.gbm", distribution = "multinomial", n.trees = 1000,
  predict.type = "prob")
gbm.mod = train(gbm.lrn, iris.task, subset = train.sub)
#performance(predict(gbm.mod, iris.task, subset = !train.sub), measures = list(mmce, acc))

plotPartialDependence(generatePartialDependenceData(gbm.mod, iris.task))
# Petal.Width: values >.5, why is the probability of setosa still .3. It should be more like zero
# Petal.Length: prob of setosa more plausible (zero for values )
gbm.ALE = computeALE(gbm.mod$learner.model, iris, "Petal.Width", K = 10, multiclass = TRUE,
  predict.fun = function(object, newdata) predict(object, newdata, type = "response", n.trees = 1000)[, , 1])
ggplot(data = gbm.ALE$ale.plot.data, aes(x = x, y = probability, group = class, col = class)) +
  geom_line() + geom_point()
# negative probabilties; prob of setosa does not get down at all
ALEPlot::ALEPlot(iris[-5], gbm.mod$learner.model,
  pred.fun = function(X.model, newdata) predict(X.model, newdata, type = "response", n.trees = 1000)[, 1, 1],
  J = 4, K = 10)
# scale on original ALEPlot function not useful

# Petal.Width:
gbm.LPD.petwid = computePD(gbm.mod$learner.model, iris, "Petal.Width", n = 20, l = 15, multiclass = TRUE,
  predict.fun = function(object, newdata) predict(object, newdata, type = "response", n.trees = 1000)[, , 1])
plotPD(gbm.LPD.petwid)
plotPartialDependence(generatePartialDependenceData(gbm.mod, iris.task, features = "Petal.Width", n = 20))
# again local partial dependence much better

# Petal.Length:
gbm.LPD.petlen = computePD(gbm.mod$learner.model, iris, "Petal.Length", n = 40, l = 15, multiclass = TRUE,
  predict.fun = function(object, newdata) predict(object, newdata, type = "response", n.trees = 1000)[, , 1])
plotPD(gbm.LPD.petlen)
plotPartialDependence(generatePartialDependenceData(gbm.mod, iris.task, features = "Petal.Length", n = 40))


####################-
# Random Forest ####
rf.lrn = makeLearner("classif.randomForest", predict.type = "prob", ntree = 1000)
rf.mod = train(rf.lrn, iris.task, subset = train.sub)
#performance(predict(rf.mod, iris.task, subset = !train.sub), measures = list(mmce, acc))
plotPartialDependence(generatePartialDependenceData(rf.mod, iris.task))
# here probability of setosa drops for both petal.length and petal.width
test = predict(rf.mod$learner.model, newdata = iris, type = "prob")

rf.LPD.petwid = computePD(rf.mod$learner.model, iris, "Petal.Width", n = 20, l = 15, multiclass = TRUE,
  predict.fun = function(object, newdata) predict(object, newdata, type = "prob"))
plotPD(rf.LPD.petwid)
plotPartialDependence(generatePartialDependenceData(rf.mod, iris.task, features = "Petal.Width", n = 20))


##########-
# SVM ####
svm.lrn = makeLearner("classif.svm", predict.type = "prob")
svm.mod = train(svm.lrn, iris.task, subset = train.sub)
#performance(predict(svm.mod, iris.task, subset = !train.sub), measures = list(mmce, acc))
plotPartialDependence(generatePartialDependenceData(svm.mod, iris.task))

svm.ALE = computeALE(svm.mod$learner.model, iris, "Petal.Width", K = 10, multiclass = TRUE,
  predict.fun = function(object, newdata) attributes(predict(object, newdata, probability = TRUE))[["probabilities"]])
ggplot(data = svm.ALE$ale.plot.data, aes(x = x, y = probability, group = class, col = class)) +
  geom_line() + geom_point()
# negative probabilities

svm.LPD.petwid = computePD(svm.mod$learner.model, iris, "Petal.Width", n = 10, l = 15, multiclass = TRUE,
  predict.fun = function(object, newdata) attributes(predict(object, newdata, probability = TRUE))[["probabilities"]])
plotPD(svm.LPD.petwid)
plotPartialDependence(generatePartialDependenceData(svm.mod, iris.task, features = "Petal.Width", n = 10))

# compare to simply plotting the predictions:
svm.predict.probs = attributes(predict(svm.mod$learner.model, iris, probability = TRUE))[["probabilities"]]
svm.predict.data = reshape2::melt(data.frame(x = iris$Petal.Width, svm.predict.probs), id.vars = "x", variable.name = "class", value.name = "probability")
ggplot(data = svm.predict.data, aes(x = x, y = probability, group = class, col = class)) +
  geom_point() + geom_line()
