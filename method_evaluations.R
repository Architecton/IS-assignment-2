library(CORElearn)

# Prompt user to select dataset to use.
dataset_sel <- NULL
feature_sel <- NULL
dataset <- NULL
while (TRUE) {
  dataset_sel <- readline(prompt="Enter dataset to use (1 or 2): ")
  if (dataset_sel == "1") {
    dataset <- readRDS("dataset1.rds")
    feature_sel <- readRDS("data/feature_sel_struct.rds")
    break
  } else if (dataset_sel == "2") {
    dataset <- readRDS("dataset2.rds")
    feature_sel <- readRDS("data/feature_sel_vocab.rds")
    break
  } else {
    cat(sprintf("Unrecognized dataset index '%s'. Please try again.", dataset_sel))
  }
}

# Promp user to select method to use and target variable
method_sel <- NULL
while (TRUE) {
  method_sel <- readline(prompt="enter classification method: ")
  if (length(intersect(method_sel, c('ksvm', 'voting', 'weighted_voting', 'bagging', 'random_forest', 'boosting'))) != 0) {
    break
  } else {
    cat(sprintf("Unrecognized classification method '%s'. Please try again.\n", method_sel))
  }
}

target_sel <- NULL
while (TRUE) {
  target_sel <- readline(prompt="enter target variable name: ")
  if (target_sel == 'vocabulary' || target_sel == 'structure') {
    break;
  } else {
    cat(sprintf("Unrecognized target variable '%s'. Please try again.\n", target_sel))
  }
}

# Parse values of target variables.
structure <- read.table('structure.txt')
vocabulary <- read.table('vocabulary.txt')

if (target_sel == 'vocabulary') {
  class_vals = vocabulary
} else {
  class_vals = structure
}

# Select features using selector vector stored in feature_sel.rds
dataset <- dataset[,feature_sel]
dataset <- cbind(dataset, class_vals)
names(dataset)[ncol(dataset)] <- "class_vals"
dataset[ncol(dataset)] <- factor(dataset$class_vals, levels=c(1,2,3,4,5,6))

# source the cross validation function
source("cross_validate.R")



############################################################################################
#      __  __      _   _               _   ______          _             _   _             #
#     |  \/  |    | | | |             | | |  ____|        | |           | | (_)            #
#    | \  / | ___| |_| |__   ___   __| | | |____   ____ _| |_   _  __ _| |_ _  ___  _ __   #
#   | |\/| |/ _ \ __| '_ \ / _ \ / _` | |  __\ \ / / _` | | | | |/ _` | __| |/ _ \| '_ \   #
#  | |  | |  __/ |_| | | | (_) | (_| | | |___\ V / (_| | | |_| | (_| | |_| | (_) | | | |   #
# |_|  |_|\___|\__|_| |_|\___/ \__,_| |______\_/ \__,_|_|\__,_|\__,_|\__|_|\___/|_| |_|    #
############################################################################################

## ksvm ##############################################################################################################################

if (method_sel == 'ksvm') {
  library(kernlab)
  kernels <- c('rbfdot', 'polydot', 'vanilladot', 'tanhdot', 'laplacedot', 'besseldot', 'anovadot', 'splinedot', 'stringdot')
  
  CAs_ksvm <- double(length(kernels))
  for (k in seq_along(kernels)) {
    prediction <- factor(cross_validate(dataset, target_sel, 5, 'ksvm', kernels[k]), levels=c(1, 2, 3, 4, 5, 6))
    t <- table(dataset$class_vals, prediction)  
    CA <- sum(diag(t))/sum(t)
    CAs_ksvm[k] <- CA
  }
}

#######################################################################################################################################


## Voting #############################################################################################################################

if (method_sel == 'voting') {
  prediction <- factor(cross_validate(dataset, target_sel, 5, 'voting'), levels=c(1, 2, 3, 4, 5, 6))
  t <- table(dataset$class_vals, prediction)  
  CA_voting <- sum(diag(t))/sum(t)
}

#######################################################################################################################################

## Weighted voting ####################################################################################################################

if (method_sel == 'weighted_voting') {
  prediction <- factor(cross_validate(dataset, target_sel, 5, 'weighted_voting'), levels=c(1, 2, 3, 4, 5, 6))
  t <- table(dataset$class_vals, prediction)  
  CA_weighted_voting <- sum(diag(t))/sum(t)
}



## Bagging #############################################################################################################################

if (method_sel == 'bagging') {
  prediction <- factor(cross_validate(dataset, target_sel, 5, 'bagging'), levels=c(1, 2, 3, 4, 5, 6))
  t <- table(dataset$class_vals, prediction)  
  CA_bagging <- sum(diag(t))/sum(t)
}

########################################################################################################################################



## Random forest as a variation of bagging ##############################################################################################

if (method_sel == 'random_forest') {
  prediction <- factor(cross_validate(dataset, target_sel, 5, 'random_forest'), levels=c(1, 2, 3, 4, 5, 6))
  t <- table(dataset$class_vals, prediction)  
  CA_random_forest <- sum(diag(t))/sum(t)
}

########################################################################################################################################


## Boosting ############################################################################################################################

if (method_sel == 'boosting') {
  prediction <- factor(cross_validate(dataset, target_sel, 5, 'boosting'), levels=c(1, 2, 3, 4, 5, 6))
  t <- table(dataset$class_vals, prediction)  
  CA_boosting <- sum(diag(t))/sum(t)
}

########################################################################################################################################