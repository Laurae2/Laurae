#' Cascade Forest Predictor implementation in R
#'
#' This function attempts to predict from Cascade Forest using xgboost.
#' 
#' For implementation details of Cascade Forest / Complete-Random Tree Forest / Multi-Grained Scanning / Deep Forest, check this: \url{https://github.com/Microsoft/LightGBM/issues/331#issuecomment-283942390} by Laurae.
#' 
#' @param model Type: list. A model trained by \code{CascadeForest}.
#' @param data Type: data.table. A data to predict on. If passing training data, it will predict as if it was out of fold and you will overfit (so, use the list \code{train_preds} instead please).
#' @param folds Type: list. The folds as list for cross-validation if using the training data. Otherwise, leave \code{NULL}. Defaults to \code{NULL}.
#' @param layer Type: numeric. The layer you want to predict on. If not provided (\code{NULL}), attempts to guess by taking the last layer of the model. Defaults to \code{NULL}.
#' @param prediction Type: logical. Whether the predictions of the forest ensemble are averaged. Set it to \code{FALSE} for debugging / feature engineering. Setting it to \code{TRUE} overrides \code{return_list}. Defaults to \code{TRUE}.
#' @param multi_class Type: numeric. How many classes you got. Set to 2 for binary classification, or regression cases. Set to \code{NULL} to let it try guessing by reading the \code{model}. Defaults to \code{NULL}.
#' @param data_start Type: vector of numeric. The initial prediction labels. Set to \code{NULL} if you do not know what you are doing. Defaults to \code{NULL}.
#' @param return_list Type: logical. Whether lists should be returned instead of concatenated frames for predictions. Defaults to \code{TRUE}.
#' @param low_memory Type: logical. Whether to perform the data.table transformations in place to lower memory usage. Defaults to \code{FALSE}.
#' 
#' @return A data.table or a list based on \code{data} predicted using \code{model}.
#' 
#' @examples
#' \dontrun{
#' # Load libraries
#' library(data.table)
#' library(Matrix)
#' library(xgboost)
#' 
#' # Create data
#' data(agaricus.train, package = "lightgbm")
#' data(agaricus.test, package = "lightgbm")
#' agaricus_data_train <- data.table(as.matrix(agaricus.train$data))
#' agaricus_data_test <- data.table(as.matrix(agaricus.test$data))
#' agaricus_label_train <- agaricus.train$label
#' agaricus_label_test <- agaricus.test$label
#' folds <- Laurae::kfold(agaricus_label_train, 5)
#' 
#' # Train a model (binary classification)
#' model <- CascadeForest(training_data = agaricus_data_train, # Training data
#'                        validation_data = agaricus_data_test, # Validation data
#'                        training_labels = agaricus_label_train, # Training labels
#'                        validation_labels = agaricus_label_test, # Validation labels
#'                        folds = folds, # Folds for cross-validation
#'                        boosting = FALSE, # Do not touch this unless you are expert
#'                        nthread = 1, # Change this to use more threads
#'                        cascade_lr = 1, # Do not touch this unless you are expert
#'                        training_start = NULL, # Do not touch this unless you are expert
#'                        validation_start = NULL, # Do not touch this unless you are expert
#'                        cascade_forests = rep(4, 5), # Number of forest models
#'                        cascade_trees = 10, # Number of trees per forest
#'                        cascade_rf = 2, # Number of Random Forest in models
#'                        cascade_seeds = 0, # Seed per layer
#'                        objective = "binary:logistic",
#'                        eval_metric = Laurae::df_logloss,
#'                        multi_class = 2, # Modify this for multiclass problems
#'                        early_stopping = 2, # stop after 2 bad combos of forests
#'                        maximize = FALSE, # not a maximization task
#'                        verbose = TRUE, # print information during training
#'                        low_memory = FALSE)
#' 
#' # Predict from model
#' new_preds <- CascadeForest_pred(model, agaricus_data_test, prediction = FALSE)
#' 
#' # We can check whether we have equal predictions, it's all TRUE!
#' all.equal(model$train_means, CascadeForest_pred(model,
#'                                                 agaricus_data_train,
#'                                                 folds = folds))
#' all.equal(model$valid_means, CascadeForest_pred(model,
#'                                                 agaricus_data_test))
#' 
#' # Attempt to perform fake multiclass problem
#' agaricus_label_train[1:100] <- 2
#' 
#' # Train a model (multiclass classification)
#' model <- CascadeForest(training_data = agaricus_data_train, # Training data
#'                        validation_data = agaricus_data_test, # Validation data
#'                        training_labels = agaricus_label_train, # Training labels
#'                        validation_labels = agaricus_label_test, # Validation labels
#'                        folds = folds, # Folds for cross-validation
#'                        boosting = FALSE, # Do not touch this unless you are expert
#'                        nthread = 1, # Change this to use more threads
#'                        cascade_lr = 1, # Do not touch this unless you are expert
#'                        training_start = NULL, # Do not touch this unless you are expert
#'                        validation_start = NULL, # Do not touch this unless you are expert
#'                        cascade_forests = rep(4, 5), # Number of forest models
#'                        cascade_trees = 10, # Number of trees per forest
#'                        cascade_rf = 2, # Number of Random Forest in models
#'                        cascade_seeds = 0, # Seed per layer
#'                        objective = "multi:softprob",
#'                        eval_metric = Laurae::df_logloss,
#'                        multi_class = 3, # Modify this for multiclass problems
#'                        early_stopping = 2, # stop after 2 bad combos of forests
#'                        maximize = FALSE, # not a maximization task
#'                        verbose = TRUE, # print information during training
#'                        low_memory = FALSE)
#' 
#' # Predict from model for mutliclass problems
#' new_preds <- CascadeForest_pred(model, agaricus_data_test, prediction = FALSE)
#' 
#' # We can check whether we have equal predictions, it's all TRUE!
#' all.equal(model$train_means, CascadeForest_pred(model,
#'                                                 agaricus_data_train,
#'                                                 folds = folds))
#' all.equal(model$valid_means, CascadeForest_pred(model,
#'                                                 agaricus_data_test))
#' }
#' 
#' @export

CascadeForest_pred <- function(model,
                               data,
                               folds = NULL,
                               layer = NULL,
                               prediction = TRUE,
                               multi_class = NULL,
                               data_start = NULL,
                               return_list = FALSE,
                               low_memory = FALSE) {
  
  preds <- list()
  
  # Reverse engineer multi_class
  if (is.null(multi_class)) {
    if (is.null(model$multi_class)) {
      multi_class <- 2 # Attempt to guess...
    } else {
      multi_class <- model$multi_class
    }
  }
  
  # Reverse engineer layer
  if (is.null(layer)) {
    if (!is.null(model$best_iteration)) {
      layer <- model$best_iteration
    } else {
      layer <- length(model$model)
    }
  }
  
  # Check if only one layer
  if (layer > 1) {
    
    # Do first predictions
    preds <- CRTreeForest_pred(model = model$model[[1]],
                               data = data,
                               folds = folds,
                               prediction = FALSE,
                               multi_class = multi_class,
                               data_start = data_start,
                               return_list = FALSE)
    
    # Check for low memory requirements
    if (low_memory == TRUE) {
      
      # Store column count
      original_cols <- copy(ncol(data))
      
      # Append predictions
      data <- Laurae::DTcbind(data, preds)
      
      # Check for more than 2 layers to loop through all layers except last
      if (layer > 2) {
        
        # Loop through all layers except last
        for (i in 2:(layer - 1)) {
          
          # Do intermediary predictions
          preds <- CRTreeForest_pred(model = model$model[[i]],
                                     data = data,
                                     folds = folds,
                                     prediction = FALSE,
                                     multi_class = multi_class,
                                     data_start = data_start,
                                     return_list = FALSE)
          
          # Overwrite predictions
          data <- Laurae::DTcbind(data, preds)
          
        }
        
      }
      
      # Do final predictions
      preds <- CRTreeForest_pred(model = model$model[[layer]],
                                 data = data,
                                 folds = folds,
                                 prediction = prediction,
                                 multi_class = multi_class,
                                 data_start = data_start,
                                 return_list = return_list)
      
      # Restore original data
      DTcolsample(data, kept = original_cols:copy(ncol(data)), remove = TRUE, low_mem = TRUE)
      
    } else {
      
      # Copy data
      new_data <- copy(data)
      
      # Append predictions
      new_data <- Laurae::DTcbind(new_data, preds)
      
      # Check for more than 2 layers to loop through all layers except last
      if (layer > 2) {
        
        # Loop through all layers except last
        for (i in 2:(layer - 1)) {
          
          # Do intermediary predictions
          preds <- CRTreeForest_pred(model = model$model[[i]],
                                     data = new_data,
                                     folds = folds,
                                     prediction = FALSE,
                                     multi_class = multi_class,
                                     data_start = data_start,
                                     return_list = FALSE)
          
          # Overwrite predictions
          new_data <- Laurae::DTcbind(new_data, preds)
          
        }
        
      }
      
      # Do final predictions
      preds <- CRTreeForest_pred(model = model$model[[layer]],
                                 data = new_data,
                                 folds = folds,
                                 prediction = prediction,
                                 multi_class = multi_class,
                                 data_start = data_start,
                                 return_list = return_list)
      
    }
    
  } else {
    
    # Do final predictions
    preds <- CRTreeForest_pred(model = model$model[[1]],
                               data = data,
                               folds = folds,
                               prediction = prediction,
                               multi_class = multi_class,
                               data_start = data_start,
                               return_list = return_list)
    
  }
  
  
  return(preds)
  
}
