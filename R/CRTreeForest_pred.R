#' Complete-Random Tree Forest Predictor implementation in R
#'
#' This function attempts to predict from Complete-Random Tree Forests using xgboost. Predictions are deferred to \code{CRTreeForest_pred_internals}.
#' 
#' For implementation details of Cascade Forest / Complete-Random Tree Forest / Multi-Grained Scanning / Deep Forest, check this: \url{https://github.com/Microsoft/LightGBM/issues/331#issuecomment-283942390} by Laurae.
#' 
#' @param model Type: list. A model trained by \code{CRTreeForest}.
#' @param data Type: data.table. A data to predict on. If passing training data, it will predict as if it was out of fold and you will overfit (so, use the list \code{train_preds} instead please).
#' @param folds Type: list. The folds as list for cross-validation if using the training data. Otherwise, leave \code{NULL}. Defaults to \code{NULL}.
#' @param prediction Type: logical. Whether the predictions of the forest ensemble are averaged. Set it to \code{FALSE} for debugging / feature engineering. Setting it to \code{TRUE} overrides \code{return_list}. Defaults to \code{FALSE}.
#' @param multi_class Type: numeric. How many classes you got. Set to 2 for binary classification, or regression cases. Set to \code{NULL} to let it try guessing by reading the \code{model}. Defaults to \code{NULL}.
#' @param data_start Type: vector of numeric. The initial prediction labels. Set to \code{NULL} if you do not know what you are doing. Defaults to \code{NULL}.
#' @param return_list Type: logical. Whether lists should be returned instead of concatenated frames for predictions. Defaults to \code{TRUE}.
#' @param work_dir Type: character, without slash at end (ex: "dev/tools/save_in_this_folder"). The working directory where models are stored, if using external model files as memory. Defaults to \code{NULL}, which means models are in memory. It will attempt to detect automatically the working directory from the model if it is available.
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
#' model <- CRTreeForest(training_data = agaricus_data_train, # Training data
#'                       validation_data = agaricus_data_test, # Validation data
#'                       training_labels = agaricus_label_train, # Training labels
#'                       validation_labels = agaricus_label_test, # Validation labels
#'                       folds = folds, # Folds for cross-validation
#'                       nthread = 1, # Change this to use more threads
#'                       lr = 1, # Do not touch this unless you are expert
#'                       training_start = NULL, # Do not touch this unless you are expert
#'                       validation_start = NULL, # Do not touch this unless you are expert
#'                       n_forest = 5, # Number of forest models
#'                       n_trees = 10, # Number of trees per forest
#'                       random_forest = 2, # We want only 2 random forest
#'                       seed = 0,
#'                       objective = "binary:logistic",
#'                       eval_metric = Laurae::df_logloss,
#'                       return_list = TRUE, # Set this to FALSE for a data.table output
#'                       multi_class = 2, # Modify this for multiclass problems
#'                       verbose = " ")
#' 
#' # Predict from model
#' new_preds <- CRTreeForest_pred(model, agaricus_data_test, return_list = FALSE)
#' 
#' # We can check whether we have equal predictions, it's all TRUE!
#' all.equal(model$train_preds, CRTreeForest_pred(model, agaricus_data_train, folds = folds))
#' all.equal(model$valid_preds, CRTreeForest_pred(model, agaricus_data_test))
#' all.equal(model$train_means, CRTreeForest_pred(model,
#'                                                agaricus_data_train,
#'                                                folds = folds,
#'                                                return_list = FALSE,
#'                                                prediction = TRUE))
#' all.equal(model$valid_means, CRTreeForest_pred(model,
#'                                                agaricus_data_test,
#'                                                return_list = FALSE,
#'                                                prediction = TRUE))
#' 
#' # Attempt to perform fake multiclass problem
#' agaricus_label_train[1:100] <- 2
#' 
#' # Train a model (multiclass classification)
#' model <- CRTreeForest(training_data = agaricus_data_train, # Training data
#'                       validation_data = agaricus_data_test, # Validation data
#'                       training_labels = agaricus_label_train, # Training labels
#'                       validation_labels = agaricus_label_test, # Validation labels
#'                       folds = folds, # Folds for cross-validation
#'                       nthread = 1, # Change this to use more threads
#'                       lr = 1, # Do not touch this unless you are expert
#'                       training_start = NULL, # Do not touch this unless you are expert
#'                       validation_start = NULL, # Do not touch this unless you are expert
#'                       n_forest = 5, # Number of forest models
#'                       n_trees = 10, # Number of trees per forest
#'                       random_forest = 2, # We want only 2 random forest
#'                       seed = 0,
#'                       objective = "multi:softprob",
#'                       eval_metric = Laurae::df_logloss,
#'                       return_list = TRUE, # Set this to FALSE for a data.table output
#'                       multi_class = 3, # Modify this for multiclass problems
#'                       verbose = " ")
#' 
#' # Predict from model for mutliclass problems
#' new_preds <- CRTreeForest_pred(model, agaricus_data_test, return_list = FALSE)
#' 
#' # We can check whether we have equal predictions, it's all TRUE!
#' all.equal(model$train_preds, CRTreeForest_pred(model, agaricus_data_train, folds = folds))
#' all.equal(model$valid_preds, CRTreeForest_pred(model, agaricus_data_test))
#' all.equal(model$train_means, CRTreeForest_pred(model,
#'                                                agaricus_data_train,
#'                                                folds = folds,
#'                                                return_list = FALSE,
#'                                                prediction = TRUE))
#' all.equal(model$valid_means, CRTreeForest_pred(model,
#'                                                agaricus_data_test,
#'                                                return_list = FALSE,
#'                                                prediction = TRUE))
#' }
#' 
#' @export

CRTreeForest_pred <- function(model,
                              data,
                              folds = NULL,
                              prediction = FALSE,
                              multi_class = NULL,
                              data_start = NULL,
                              return_list = TRUE,
                              work_dir = NULL) {
  
  # Reverse engineer multi_class
  if (is.null(multi_class)) {
    if (is.null(model$multi_class)) {
      multi_class <- 2 # Attempt to guess...
    } else {
      multi_class <- model$multi_class
    }
  }
  
  # Reverse engineer working directory then do predictions
  if (is.null(work_dir)) {
    
    # Do we have really a working directory used?
    if (length(model$work_dir[[1]]) > 0) {
      
      model_path <- model$work_dir[[2]] # Override from model
      out_of_memory <- TRUE
      
    } else {
      
      out_of_memory <- FALSE
      
    }
    
  } else {
    
    # Do we have really a working directory used?
    if (length(work_dir[[1]]) > 0) {
      
      model_path <- work_dir[[2]] # Override from model
      out_of_memory <- TRUE
      
    } else {
      
      out_of_memory <- FALSE
      
    }
    
  }
  
  # Load models from memory if needed
  if (out_of_memory) {
    
    # Copy model
    model_temp <- model
    
    # Loop through each forest
    for (i in 1:length(model$model)) {
      
      # Loop through each fold
      for (j in 1:length(model$model[[i]])) {
        
        # Restore model
        model_temp$model[[i]][[j]] <- xgb.load(model_path[[i]][[j]])
        
      }
      
    }
    
    # Give back hand to user
    return(CRTree_Forest_pred_internals(model = model_temp,
                                        data = data,
                                        folds = folds,
                                        prediction = prediction,
                                        multi_class = multi_class,
                                        data_start = data_start,
                                        return_list = return_list))
    
  } else {
    
    # Give back hand to user
    return(CRTree_Forest_pred_internals(model = model,
                                        data = data,
                                        folds = folds,
                                        prediction = prediction,
                                        multi_class = multi_class,
                                        data_start = data_start,
                                        return_list = return_list))
    
  }
  
}
