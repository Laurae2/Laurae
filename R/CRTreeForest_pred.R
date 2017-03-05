#' Complete-Random Tree Forest Predictor implementation in R
#'
#' This function attempts to predict from Complete-Random Tree Forests using xgboost.
#' 
#' For implementation details of Cascade Forest / Complete-Random Tree Forest / Multi-Grained Scanning / Deep Forest, check this: \url{https://github.com/Microsoft/LightGBM/issues/331#issuecomment-283942390} by Laurae.
#' 
#' @param model Type: list. A model trained by \code{CRTreeForest}.
#' @param data Type: data.table. A data to predict on. If passing training data, it will predict as if it was out of fold and you will overfit (so, use the list \code{train_preds} instead please).
#' @param prediction Type: logical. Whether the predictions of the forest ensemble are averaged. Set it to \code{FALSE} for debugging / feature engineering. Setting it to \code{TRUE} overrides \code{return_list}. Defaults to \code{FALSE}.
#' @param multi_class Type: numeric. How many classes you got. Set to 2 for binary classification, or regression cases. Set to \code{NULL} to let it try guessing by reading the \code{model}. Defaults to \code{NULL}.
#' @param data_start Type: vector of numeric. The initial prediction labels. Set to \code{NULL} if you do not know what you are doing. Defaults to \code{NULL}.
#' @param return_list Type: logical. Whether lists should be returned instead of concatenated frames for predictions. Defaults to \code{TRUE}.
#' 
#' @return A data.table or a list based on \code{data} predicted using \code{model}.
#' 
#' @examples
#' \dontrun{
#' # Load libraries
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
#'                       n_trees = 1000, # Number of trees per forest
#'                       random_forest = 2, # We want only 2 random forest
#'                       seed = 0,
#'                       objective = "binary:logistic",
#'                       eval_metric = "logloss",
#'                       return_list = TRUE, # Set this to FALSE for a data.table output
#'                       multi_class = 2, # Modify this for multiclass problems
#'                       verbose = " ")
#' 
#' # Predict from model
#' new_preds <- CRTreeForest_pred(model, agaricus_data_test, return_list = FALSE)
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
#'                       n_trees = 1000, # Number of trees per forest
#'                       random_forest = 2, # We want only 2 random forest
#'                       seed = 0,
#'                       objective = "multi:softprob",
#'                       eval_metric = "mlogloss",
#'                       return_list = TRUE, # Set this to FALSE for a data.table output
#'                       multi_class = 3, # Modify this for multiclass problems
#'                       verbose = " ")
#' 
#' # Predict from model for mutliclass problems
#' new_preds <- CRTreeForest_pred(model, agaricus_data_test, return_list = FALSE)
#' }
#' 
#' @export

CRTreeForest_pred <- function(model,
                              data,
                              prediction = FALSE,
                              multi_class = NULL,
                              data_start = NULL,
                              return_list = TRUE) {
  
  preds <- list()
  
  # Reverse engineer multi_class
  if (is.null(multi_class)) {
    if (is.null(model$multi_class)) {
      multi_class <- 2 # Attempt to guess...
    } else {
      multi_class <- model$multi_class
    }
  }
  
  # Do predictions
  for (i in 1:length(model$model)) {
    
    # Are we doing multiclass?
    if (multi_class > 2) {
      preds[[i]] <- data.table(matrix(rep(0, nrow(data) * multi_class), nrow = nrow(data), ncol = multi_class))
    } else {
      preds[[i]] <- numeric(nrow(data))
    }
    
    # Column sampling
    new_data <- Laurae::DTcolsample(data, model$features[[i]])
    new_data <- xgb.DMatrix(data = Laurae::DT2mat(new_data), base_margin = data_start)
    
    for (j in 1:length(model$model[[i]])) {
      preds[[i]] <- (predict(model$model[[i]][[j]], new_data, reshape = TRUE) / length(model$folds)) + preds[[i]]
    }
    
  }
  
  # Rename columns
  names(preds) <- paste0("Forest_", sprintf(paste0("%0", floor(log10(length(model$model))) + 1, "d"), 1:length(model$model)))
  
  # Do we want data.tables instead of lists?
  if ((return_list == FALSE) | (prediction == TRUE)) {
    
    # Is the problem a multiclass problem? (exports list of data.table instead of list of vector)
    if (multi_class > 2) {
      
      # Rename each column
      for (i in 1:length(model$model)) {
        colnames(preds[[i]]) <- paste0("Forest_", sprintf(paste0("%0", floor(log10(length(model$model))) + 1, "d"), i), "_", sprintf(paste0("%0", floor(log10(ncol(preds[[i]]))) + 1, "d"), 1:ncol(preds[[i]])))
      }
      
      train_dt <- preds[[1]]
      
      # Do we have more than one model in forest?
      if (length(model$model) > 1) {
        
        # Attempt to bind each data.table together
        for (i in 2:length(model$model)) {
          train_dt <- Laurae::DTcbind(train_dt, preds[[i]])
        }
      }
      
      if (prediction == TRUE) {
        
        # Create fresh table
        preds <- data.table(matrix(rep(0, nrow(data) * multi_class), nrow = nrow(data), ncol = multi_class))
        colnames(preds) <- paste0("Label_", sprintf(paste0("%0", floor(log10(multi_class)) + 1, "d"), 1:multi_class))
        
        # Get predictions
        for (j in 1:multi_class) {
          preds[[j]] <- rowMeans(train_dt[, (0:(length(model$model) - 1)) * multi_class + j, with = FALSE])
        }
        
      } else {
        
        # Table to return
        preds <- train_dt
        
      }
      
    } else {
      
      # Only vectors, so we can cbindlist directly
      preds <- Laurae::cbindlist(preds)
      
      if (prediction == TRUE) {
        
        # Get predictions
        preds <- rowMeans(preds)
        
      }
      
    }
    
  }
  
  return(preds)
  
}
