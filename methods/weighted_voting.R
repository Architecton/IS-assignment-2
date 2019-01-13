weighted_voting <- function(train, test) {
  
  CA <- function(observed, predicted)
  {
    t <- table(observed, predicted)
    
    sum(diag(t)) / sum(t)
  }
  
  modelDT <- CoreModel(class_vals ~ ., train, model="tree")
  modelNB <- CoreModel(class_vals ~ ., train, model="bayes")
  modelKNN <- CoreModel(class_vals ~ ., train, model="knn", kInNN = 5)
  
  predDT <- predict(modelDT, test, type = "class")
  caDT <- CA(test$class_vals, predDT)
  
  predNB <- predict(modelNB, test, type="class")
  caNB <- CA(test$class_vals, predNB)
  caNB
  
  predKNN <- predict(modelKNN, test, type="class")
  caKNN <- CA(test$class_vals, predKNN)
  
  predDT.prob <- predict(modelDT, test, type="probability")
  predNB.prob <- predict(modelNB, test, type="probability")
  predKNN.prob <- predict(modelKNN, test, type="probability")
  
  # combine predictions into a data frame
  pred.prob <- caDT * predDT.prob + caNB * predNB.prob + caKNN * predKNN.prob
  
  # pick the class with the highest score
  highest <- apply(pred.prob, 1, which.max)
  classes <- levels(train$class_vals)
  return (classes[highest])
}