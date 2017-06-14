#
# features = c("age")
#
#
# margins.ame = suppressWarnings(summary(margins::margins(margex.mod, data = margex))$AME)
#
# for (feat in features) {
#   mean(derivative(margex[[feat]], margex.mod, data = margex, feature = feat,
#     predict.fun = function(object, newdata)
#       predict(object, newdata = newdata, type = "response")))
#
# }
# long = lapply(margex[["age"]], function(x) {
#   derivative(x, margex.mod, data = margex, feature = c("age"),
#     predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "response"))
# })
