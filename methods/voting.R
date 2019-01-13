voting <- function(train, test) {
  modelDT <- CoreModel(class_vals ~ ., train, model="tree")
  modelNB <- CoreModel(class_vals ~ ., train, model="bayes")
  modelKNN <- CoreModel(class_vals ~ ., train, model="knn", kInNN = 5)
  
  predDT <- predict(modelDT, test, type="class")
  predNB <- predict(modelNB, test, type="class")
  predKNN <- predict(modelKNN, test, type="class")
  
  # combine predictions into a data frame
  pred <- data.frame(predDT, predNB, predKNN)
  
  # the class with the most votes wins
  voting <- function(predictions)
  {
    res <- vector()
    
    for (i in 1 : nrow(predictions))  	
    {
      vec <- unlist(predictions[i,])
      res[i] <- names(which.max(table(vec)))
    }
    
    factor(res, levels=levels(predictions[,1]))
  }
  
  return(voting(pred))
}