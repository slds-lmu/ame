library(mlr)

margex.task = mlr::makeClassifTask(data = margex, target = "outcome")

# fit classification model
margex.form = outcome ~ treatment*group + age + I(age^2) + treatment:age
margex.mod = glm(margex.form, data = margex, family = binomial)


