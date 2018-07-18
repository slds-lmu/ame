
n = 40
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
y5 = function(x) -4 * cos(4*pi*x) * x

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
