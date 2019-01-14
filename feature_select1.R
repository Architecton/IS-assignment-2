# Set working directory.
wd <- "/home/jernej/Desktop/Repositories/projects/IS-assignment-2"
setwd(wd)

# Parse dataset.
dataset <- readRDS("data/dataset1.rds")

# Parse grading results of essay vocabulary.
vocabulary <- read.table('vocabulary.txt')

## FEATURE SELECTION WITH A GENETIC ALGORITHM #######################################
library('kernlab')
library('GA')
source('cross_validate.R')

# feature_selection: perform feature selection over the dataset using genetic algorithms
feature_selection <- function(dataset, target, k, kernel, class_vals) {
  dataset_eval <- cbind(dataset, class_vals)  # bind class values to dataset.
  names(dataset_eval)[ncol(dataset_eval)] <- "class_vals"  # Name column containing class values.
  dataset_eval[ncol(dataset_eval)] <- factor(dataset_eval$class_vals, levels=c(1,2,3,4,5,6))
  
  # sel_fitness: fitness function for feature selection
  sel_fitness <- function(sel) {
    sel_dataset <- dataset[,sel]  # Apply selection encoded in chromosome.
    sel_dataset <- cbind(sel_dataset, class_vals)  # bind class values.
    names(sel_dataset)[ncol(sel_dataset)] <- "class_vals"  # give name to class column.
    sel_dataset[ncol(sel_dataset)] <- factor(sel_dataset$class_vals, levels=c(1,2,3,4,5,6))  # factor class column.
    prediction <- factor(cross_validate(sel_dataset, target, k, 'ksvm', kernel), levels=c(1,2,3,4,5,6))  # Get prediction using CA computed using CV.
    t <- table(dataset_eval$class_vals, prediction)  # Compute CV.
    CA <- sum(diag(t))/sum(t)
    return (CA)
  }
  
  # Run genetic algorithm to obtain a good seletion.
  GA <- ga(type="binary", fitness=sel_fitness, nBits=ncol(dataset), popSize=10, maxiter=10, run=5)
  return(as.integer(GA@solution))
}

# Perform feature selection using genetic algorithm and save results.
sel <- feature_selection(dataset, 'vocabulary', 5, 'rbfdot', vocabulary)
saveRDS(sel, file = "data/feature_sel_vocab.rds")