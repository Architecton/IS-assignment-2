# test_cv: test the prediction success using k-fold cross validation.
cross_validate <- function(dataset, target, k, method, spec=NULL) {
  
  # Append indices of rows to save information about the shuffling order.
  dataset_aux <- cbind(data.frame(1:nrow(dataset)), dataset)
  perm <- sample(1:nrow(dataset), nrow(dataset), replace=FALSE)
  dataset_aux <- dataset_aux[perm, ]
  num_in_group <- ceiling(nrow(dataset_aux)/k)  # Compute maximum number of samples in each group.
  pred = matrix(, nrow = 0, ncol = 1)           # Allocate starting array for prediction results matrix.
  # Go over example sets.
  for (idx in seq(1, nrow(dataset_aux), num_in_group)) {
    test_rows = dataset_aux[idx:min(idx + num_in_group - 1, nrow(dataset_aux)),]                               # Get testing rows.
    train_rows = rbind(dataset_aux[1:idx-1,], dataset_aux[min(idx + num_in_group, nrow(dataset_aux)):nrow(dataset_aux),])            # Get training rows.
    
    if (method == 'ksvm') {
      if (target == 'vocabulary') {
        model <- ksvm(class_vals ~ ., train_rows, kernel=spec) 
      } else {
        model <- ksvm(class_vals ~ ., train_rows, kernel=spec) 
      }
      predicted <- predict(model, test_rows, type="response")  
    } else if (method == 'voting') {
      source('methods/voting.R')
      predicted <- voting(train_rows, test_rows)
    } else if (method == 'weighted_voting') {
      source('methods/weighted_voting.R')
      predicted <- weighted_voting(train_rows, test_rows)
    } else if (method == 'bagging') {
      source('methods/bagging_predict.R')
      predicted <- bagging_predict(train_rows, test_rows)
    } else if (method == 'random_forest') {
      source('methods/random_forest.R')
      predicted <- random_forest(train_rows, test_rows)
    } else if (method == 'boosting') {
      source('methods/boosting_predict.R')
      predicted <- boosting_predict(train_rows, test_rows)
    }
    pred <- rbind(pred, as.matrix(predicted))  # add next set of predictions to results matrix.
  }
  pred <- pred[perm]  # Apply inverse permutation and return predictions.
  return (pred)
}