random_forest <- function(train, test) {
  library(randomForest)
  rf <- randomForest(class_vals ~ ., learn)
  return(predict(rf, test, type = "class"))
}