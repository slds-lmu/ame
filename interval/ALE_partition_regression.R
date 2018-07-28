library(mlr)
devtools::load_all()

# dgp
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

ALE = computeALE(nnet.mod$learner.model, df, "x2", K = 100)
plotALE(ALE)

plotPrediction(nnet.mod, tsk, "x2")$plot


# partition
partition(ALE$ale.x, ALE$ale, 5)
plotALE(ALE) + geom_vline(xintercept = partition(ALE$ale.x, ALE$ale, 5))
plotALE(ALE) + geom_vline(xintercept = partition(ALE$ale.x, ALE$ale, 5, part.method = "cluster"))

