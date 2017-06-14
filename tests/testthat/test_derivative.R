mean(derivative(margex[["age"]], margex.mod, data = margex, feature = c("age"),
  predict.fun = function(object, newdata)
    predict(object, newdata = newdata, type = "response")))

sort(summary(margins(margex.mod, data = margex))$AME)

# long = lapply(margex[["age"]], function(x) {
#   derivative(x, margex.mod, data = margex, feature = c("age"),
#     predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
# })
