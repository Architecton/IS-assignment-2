boosting_predict <- function(train, test) {
  library(adabag)
  bm <- boosting(class_vals ~ ., train)
  predictions <- predict(bm, test)
  return (predictions$class)
}