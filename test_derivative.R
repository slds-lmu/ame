library(mlr)

# test different data types
n = 100
bin1 = rbinom(n, 1, prob = .75)
cat1.map = setNames(c(0,-2,2,6), c("blue", "red2", "red", "green"))
cat1 = sample(factor(cat1.map, labels = names(cat1.map)), n, replace = TRUE, prob = c(.25, .25, .25, .25))
ord1.map = setNames(c(0,4,5,10,20), c("low", "midlow", "mid", "midhi", "high"))
ord1 = sample(factor(ord1.map, labels = names(ord1.map)), n, replace = TRUE, prob = c(.1, .2, .2, .3, .2))
logic1 = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(.45, .55))
char1 = sample(c("A", "E", "I"), n, replace = TRUE, prob = c(.3, .4, .3))
x1 = runif(n, -.5, .5)
eps = rnorm(n, 0, 2)
# data generating process:
y = .4 + 2.2 * x1 + 2.3 * bin1 + .1 * cat1.map[cat1] + .1 * ord1.map[ord1] + logic1 * 1.3 +
  ifelse(char1 == "A", 0, ifelse(char1 == "E", -1, 1)) + eps

df = data.frame(y, x1, bin1 = factor(bin1), cat1, ord1, char1, logic1, stringsAsFactors = FALSE)
knitr::kable(head(df))

######################
# Linear model
lm.mod = lm(y ~ ., data = df)
coef(lm.mod)
computeAME(lm.mod, df, names(df)[-1])
all.equal(unlist(computeAME(lm.mod, df, names(df)[-1])), coef(lm.mod)[-1])

regr.tsk = makeRegrTask(data = df, target = "y")
lm.lrn = makeLearner("regr.lm")
lm.mod.mlr = train(lm.lrn, regr.tsk)
lm.mod.mlr$learner.model
computeAME(lm.mod.mlr, df, names(df[-1]))

######################
# Random Forest predict does not accept changing classes of columns
rf.lrn = makeLearner("regr.randomForest")
rf.mod.mlr = train(rf.lrn, regr.tsk)
rf.pred = predict(rf.mod.mlr, regr.tsk)
computeAME(rf.mod.mlr$learner.model, df, names(df)[-1])
