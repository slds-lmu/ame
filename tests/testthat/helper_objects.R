# fit model
margex.form = outcome ~ treatment*group + age + I(age^2) + treatment:age
margex.mod = glm(margex.form, data = margex, family = binomial)
