bagging_predict = function(train, test) {
  library(ipred)
  bag <- bagging(class_vals ~ ., train, nbagg=15)
  return(predict(bag, test, type="class"))
}