# Loading the corpus and performing the preprocessing
wd <- "/home/jernej/Desktop/Repositories/projects/IS-assignment-2"
setwd(wd)

dataset <- readRDS("data/dataset2.rds")
## CLASSES ##########################################################################

# Parse grading results of essay structure
structure <- read.table('structure.txt')

## FEATURE SELECTION WITH A GENETIC ALGORITHM #######################################
library('kernlab')
library('GA')
source('cross_validate.R')

# feature_selection: perform feature selection using a genetic algorithm.
feature_selection <- function(dataset, target, k, kernel, class_vals) {
  dataset_eval <- cbind(dataset, class_vals)  # Bind class values to evaluation dataset.
  names(dataset_eval)[ncol(dataset_eval)] <- "class_vals"  # Name column containing class values.
  dataset_eval[ncol(dataset_eval)] <- factor(dataset_eval$class_vals, levels=c(1,2,3,4,5,6))  # Factor class column.
  
  # sel_fitness: fitness function for the genetic algorithm.
  sel_fitness <- function(sel) {
    sel_dataset <- dataset[,sel]  # Select columns using selector vector obtained from chromosome.
    sel_dataset <- cbind(sel_dataset, class_vals)  # Bind class column to dataset.
    names(sel_dataset)[ncol(sel_dataset)] <- "class_vals"  # Name class column.
    sel_dataset[ncol(sel_dataset)] <- factor(sel_dataset$class_vals, levels=c(1,2,3,4,5,6))  # Factor class column.
    prediction <- factor(cross_validate(sel_dataset, target, k, 'ksvm', kernel), levels=c(1,2,3,4,5,6))  # Get predictions column using CV.
    t <- table(dataset_eval$class_vals, prediction)  # Compute CA.
    CA <- sum(diag(t))/sum(t)
    return (CA)
  }
  
  # Perform genetic algorithm and return best selector vector.
  GA <- ga(type="binary", fitness=sel_fitness, nBits=ncol(dataset), popSize=1, maxiter=1, run=1)
  return(as.integer(GA@solution))
}

# Perform feature selection using genetic algorithm and save results.
sel <- feature_selection(dataset, 'structure', 5, 'rbfdot', structure)
saveRDS(sel, file = "data/feature_sel_struct.rds")