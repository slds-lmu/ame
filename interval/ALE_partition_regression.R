library(mlr)
devtools::load_all()

# dgp
set.seed(4218)
set.seed(123)
set.seed(4321)
n = 500
x = runif(n, min = 0, max = 1)
x1 = x + rnorm(n, 0, 0.05)
x2 = x + rnorm(n, 0, 0.05)
y2 = function(x) -4 * cos(4*pi*x) * x + 4
y = x1 + y2(x2) + rnorm(n, 0, .5)
df = data.frame(y, x1, x2)

# fit neural network
tsk = makeRegrTask(data = df, target = "y")
nnet.lrn = makeLearner("regr.nnet", skip = FALSE, size = 20, decay = 0.0001, maxit = 1000,
  trace = FALSE)
nnet.mod = train(nnet.lrn, tsk)

plotPrediction(nnet.mod, tsk, "x2")$plot

ALE = computeALE(nnet.mod$learner.model, df, "x2", K = 50)
plotALE(ALE)


# partition
breaks = partition(ALE$ale.x, ALE$ale, 5)
plotALE(ALE) + geom_vline(xintercept = partition(ALE$ale.x, ALE$ale, 5))
plotALE(ALE) + geom_vline(xintercept = partition(ALE$ale.x, ALE$ale, 5, part.method = "cluster"))

# compute AME given the intervals
AME = computeAMEInterval(nnet.mod$learner.model, df, "x2", breaks = breaks)
AME$AME
plotAMEInterval(AME)

# add break points "manually"
AME = computeAMEInterval(nnet.mod$learner.model, df, "x2", breaks = c(0.1, breaks))
AME$AME
plotAMEInterval(AME)

# "fully automatic"
AME2 = computeAMEInterval(nnet.mod$learner.model, df, "x2")
AME2$AME
plotAMEInterval(AME2)
AME2$bounds

# Partial Derivative instead of ALE
plotPD(computePD(nnet.mod$learner.model, df, "x2", derivative = TRUE, n = 20))
plotPartialDependence(generatePartialDependenceData(nnet.mod, tsk, "x2", n = 20))
plotPartialDependence(generatePartialDependenceData(nnet.mod, tsk, "x2", n = 20, individual = TRUE))
ALEPlot::ALEPlot(df[-1], nnet.mod$learner.model,
  pred.fun = function(X.model, newdata) predict(X.model, newdata = newdata), J = 2)
AME.PDeriv = computeAMEInterval(nnet.mod$learner.model, df, "x2", method = "PDeriv") # PDeriv
plotAMEInterval(AME.PDeriv) + title("Partial Derivative")
AME.LDeriv = computeAMEInterval(nnet.mod$learner.model, df, "x2", method = "PDeriv", l = 40) # Local PDeriv
plotAMEInterval(AME.LDeriv) + title("Local Partial Derivative, l = 40")
AME.WDeriv = computeAMEInterval(nnet.mod$learner.model, df, "x2", method = "PDeriv", w = 4) # Weighted PDeriv
plotAMEInterval(AME.WDeriv) + title("Weighted Partial Derivative, w = 4")
