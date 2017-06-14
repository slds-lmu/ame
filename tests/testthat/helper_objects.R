# fit classification model
margex.form = outcome ~ treatment*group + age + I(age^2) + treatment:age
margex.mod = glm(margex.form, data = margex, family = binomial)
margex.mod.caret = caret::train(margex.form, data = margex, method = "glm")

margex.task = mlr::makeClassifTask(data = margex, target = "outcome")

# lrn = mlr::makeLearner("classif.logreg", predict.type = "prob")
# margex.mod.mlr = mlr::train(lrn, margex.task)

#pred = predict(margex.mod.caret, margex, type = "prob")
#head(pred)

#head(predict(margex.mod, newdata = margex, type = "response"))
