devtools::load_all()
n = 200
x = runif(n, min = 0, max = 1)
x1 = x + rnorm(n, 0, 0.05)
x2 = x + rnorm(n, 0, 0.05)
x3 = normalize(0.9 * x + rnorm(n, 0, .1))
x4 = .85 * x + rnorm(n, 0, .1)
x5 = normalize(0.5 * x + rnorm(n, 0, .1))

y1 = function(x) x
y2 = function(x) 10*(x-.25)^2
y3 = function(x) -2 * x
y4 = function(x) 40 * ((x-.2)^3 - (x-.2)^2) + 4
y5 = function(x) -4 * cos(4*pi*x) * x + 4

x.grid = seq(0, 1, .01)
par(mfrow = c(1,1))
plot(x.grid, y1(x.grid), type = "l")
plot(x.grid, y2(x.grid), type = "l")
plot(x.grid, y3(x.grid), type = "l")
plot(x.grid, y4(x.grid), type = "l")
plot(x.grid, y5(x.grid), type = "l")

y = y1(x1) + y2(x2) + y3(x3) + y4(x4) + y5(x5) + rnorm(n, 0, .1)
dt = data.frame(y, x1, x2, x3, x4, x5)
cor(dt)

library(mlr)
tsk = makeRegrTask(data = dt, target = "y")
nnet.lrn = makeLearner("regr.nnet", skip = FALSE, size = 20, decay = 0.0001, maxit = 1000,
  trace = FALSE)
nnet.mod = train(nnet.lrn, tsk)

ALEPlot::ALEPlot(dt[-1], nnet.mod$learner.model, pred.fun = function(X.model, newdata)
  predict(X.model, newdata), J = 5, K = 50)
ALE = computeALE(nnet.mod$learner.model, dt, "x5", K = 50)
computeALE(nnet.mod$learner.model, dt, "x5", K = 9)
lines(ALE$x, ALE$f, col = "blue", lty = 2, lwd = 2)

nnet.pd = generatePartialDependenceData(nnet.mod, tsk, "x5", n = 40, uniform = FALSE)

x.grid = seq(0, 1, .01)
f.hat = predict(nnet.mod$learner.model, dt)
c = f.hat[which.min(dt$x5)]
plot(dt$x5, f.hat, col = "red")
lines(ALE$x, ALE$f + mean(f.hat), col = "blue", lty = 2, lwd = 2)
lines(nnet.pd$data$x5, nnet.pd$data$y, col = "green")
lines(x.grid, y5(x.grid) - y5(0) + mean(f.hat), type = "l", col = "black")
#abline(v = ALE$x, lwd = .5)
legend(0, 12, legend = c("f.hat", "ALE", "PD", "true"), fill = c("red", "blue", "green", "black"))
