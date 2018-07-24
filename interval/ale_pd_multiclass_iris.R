library(mlr)
library(ALEPlot)
library(ggplot2)
library(reshape2)
devtools::load_all()

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + geom_point()
ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, col = Species)) + geom_point()
ggplot(data = iris, aes(x = Petal.Length, y = Sepal.Length, col = Species)) + geom_point()
ggplot(data = iris, aes(x = Petal.Width, y = Sepal.Width, col = Species)) + geom_point()
ggplot(data = iris, aes(x = Petal.Length, y = Sepal.Width, col = Species)) + geom_point()
ggplot(data = iris, aes(x = Petal.Width, y = Sepal.Width, col = Species)) + geom_point()
cor(iris[-5])

# iris task
getTaskData(iris.task)
n = getTaskSize(iris.task)
train.sub = rep(TRUE, n)
train.sub[c(sample(1:50, 10), sample(1:50, 10) + 50, sample(1:50, 10) + 100)] = FALSE

nnet.lrn = makeLearner("classif.nnet", skip = FALSE, size = 20, decay = 0.0001, maxit = 1000,
  trace = FALSE, predict.type = "prob")
nnet.mod = train(nnet.lrn, iris.task, subset = train.sub)
performance(predict(nnet.mod, iris.task, subset = !train.sub), measures = list(mmce, acc))

#as.numeric(predict(nnet.mod$learner.model, iris)[,1])
pred.petwidth.seto = data.frame(x = iris$Petal.Width, y = predict(nnet.mod$learner.model, iris)[,1])
ALE = computeALE(nnet.mod$learner.model, iris, "Petal.Width", K = 20, multiclass = TRUE)
ggplot(data = ALE$ale.plot.data, aes(x = x, y = probability, group = class, col = class)) +
  geom_line() + geom_point() +
  geom_point(data = pred.petwidth.seto, mapping = aes(x = x, y = y), alpha = .2, inherit.aes = FALSE)
plotPartialDependence(generatePartialDependenceData(nnet.mod, iris.task, features = "Petal.Width", n = 20))
# does ICE plot help?
nnet.ICE = generatePartialDependenceData(nnet.mod, iris.task, features = "Petal.Width", individual = TRUE)
ggplot(subset(nnet.ICE$data, Class == "setosa"), aes(x = Petal.Width, y = Probability, group = n)) +
  geom_point() + geom_line()
# so PD averages lots of prob=1 and prob=0 --> PD missleading
nnet.ICE = generatePartialDependenceData(nnet.mod, iris.task, features = "Petal.Length", individual = TRUE)
ggplot(subset(nnet.ICE$data, Class == "setosa"), aes(x = Petal.Length, y = Probability, group = n)) +
  geom_point() + geom_line()

ALEPlot(iris[-5], nnet.mod$learner.model, pred.fun = function(X.model, newdata) predict(X.model, newdata)[,"setosa"], J = 4)


## Petal.Length
f.setosa.org = ALEPlot(iris[-5], nnet.mod$learner.model, pred.fun = function(X.model, newdata) predict(X.model, newdata)[,"setosa"], J = 3)$f
f.versi.org = ALEPlot(iris[-5], nnet.mod$learner.model, pred.fun = function(X.model, newdata) predict(X.model, newdata)[,"versicolor"], J = 3)$f
f.virgi.org = ALEPlot(iris[-5], nnet.mod$learner.model, pred.fun = function(X.model, newdata) predict(X.model, newdata)[,"virginica"], J = 3)$f
ALE = computeALE(nnet.mod$learner.model, iris, "Petal.Length", K = 40, multiclass = TRUE)
ALE$f
data.frame(setosa = f.setosa.org, versicolor = f.versi.org, virginica = f.virgi.org)

ALE = computeALE(nnet.mod$learner.model, iris, "Petal.Length", K = 10, multiclass = TRUE)
ggplot(data = ALE$ale.plot.data, aes(x = x, y = probability, group = class, col = class)) +
  geom_line() + geom_point()
plotPartialDependence(generatePartialDependenceData(nnet.mod, iris.task,
  features = "Petal.Length", n = 10, uniform = TRUE))

(f.hat.mean = colMeans(predict(nnet.mod$learner.model, iris)))

# PD all features
plotPartialDependence(generatePartialDependenceData(nnet.mod, iris.task, n = 10, uniform = TRUE))

#####################
# Gradient Boosting
gbm.lrn = makeLearner("classif.gbm", distribution = "multinomial", n.trees = 1000,
  predict.type = "prob")
gbm.mod = train(gbm.lrn, iris.task, subset = train.sub)
performance(predict(gbm.mod, iris.task, subset = !train.sub), measures = list(mmce, acc))

plotPartialDependence(generatePartialDependenceData(gbm.mod, iris.task))

# Petal.Width: values >.5, why is the probability of setosa still .3. It should be more like zero
# Petal.Length: prob of setosa more plausible (zero for values )
# Is this caused by extrapolation of Partial Dependence?
# TODO: check ALEPlot here

rf.lrn = makeLearner("classif.randomForest", predict.type = "prob", ntree = 1000)
rf.mod = train(rf.lrn, iris.task, subset = train.sub)
performance(predict(rf.mod, iris.task, subset = !train.sub), measures = list(mmce, acc))
plotPartialDependence(generatePartialDependenceData(rf.mod, iris.task))
# here probability of setosa drops for both petal.length and petal.width

rf.pred = predict(rf.mod, iris.task)
predict(rf.mod$learner.model, newdata = iris, type = "prob")
predict(gbm.mod$learner.model, newdata = iris, type = "response", n.trees = 1000)


##############################
# SVM
svm.lrn = makeLearner("classif.svm", predict.type = "prob")
svm.mod = train(svm.lrn, iris.task, subset = train.sub)
performance(predict(svm.mod, iris.task, subset = !train.sub), measures = list(mmce, acc))
plotPartialDependence(generatePartialDependenceData(svm.mod, iris.task))

svm.ALE = computeALE(nnet.mod$learner.model, iris, "Petal.Width", K = 10, multiclass = TRUE)
ggplot(data = svm.ALE$ale.plot.data, aes(x = x, y = probability, group = class, col = class)) +
  geom_line() + geom_point()
