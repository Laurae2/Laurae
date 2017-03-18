#' Multi-Grained Scanning Predictor implementation in R
#'
#' This function attempts to predict from Multi-Grained Scanning using xgboost.
#' 
#' For implementation details of Cascade Forest / Complete-Random Tree Forest / Multi-Grained Scanning / Deep Forest, check this: \url{https://github.com/Microsoft/LightGBM/issues/331#issuecomment-283942390} by Laurae.
#' 
#' @param model Type: list. A model trained by \code{MGScanning}.
#' @param data Type: data.table. A data to predict on. If passing training data, it will predict as if it was out of fold and you will overfit (so, use the list \code{preds} instead please).
#' @param folds Type: list. The folds as list for cross-validation if using the training data. Otherwise, leave \code{NULL}. Defaults to \code{NULL}.
#' @param dimensions Type: numeric. The dimensions of the data. Only supported is \code{1} for matrix format, and \code{2} for list of matrices. Defaults to \code{NULL}.
#' @param multi_class Type: numeric. How many classes you got. Set to 2 for binary classification, or regression cases. Set to \code{NULL} to let it try guessing by reading the \code{model}. Defaults to \code{NULL}.
#' @param data_start Type: vector of numeric. The initial prediction labels. Set to \code{NULL} if you do not know what you are doing. Defaults to \code{NULL}.
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
#' # Train a model (binary classification) - FAST VERSION
#' model <- MGScanning(data = agaricus_data_train, # Training data
#'                     labels = agaricus_label_train, # Training labels
#'                     folds = folds, # Folds for cross-validation
#'                     dimensions = 1, # Change this for 2 dimensions if needed
#'                     depth = 10, # Change this to change the sliding window size
#'                     stride = 20, # Change this to change the sliding window speed
#'                     nthread = 1, # Change this to use more threads
#'                     lr = 1, # Do not touch this unless you are expert
#'                     training_start = NULL, # Do not touch this unless you are expert
#'                     validation_start = NULL, # Do not touch this unless you are expert
#'                     n_forest = 2, # Number of forest models
#'                     n_trees = 30, # Number of trees per forest
#'                     random_forest = 1, # We want only 2 random forest
#'                     seed = 0,
#'                     objective = "binary:logistic",
#'                     eval_metric = Laurae::df_logloss,
#'                     multi_class = 2, # Modify this for multiclass problems)
#'                     verbose = TRUE)
#' 
#' # Train a model (binary classification) - SLOW
#' model <- MGScanning(data = agaricus_data_train, # Training data
#'                     labels = agaricus_label_train, # Training labels
#'                     folds = folds, # Folds for cross-validation
#'                     dimensions = 1, # Change this for 2 dimensions if needed
#'                     depth = 10, # Change this to change the sliding window size
#'                     stride = 1, # Change this to change the sliding window speed
#'                     nthread = 1, # Change this to use more threads
#'                     lr = 1, # Do not touch this unless you are expert
#'                     training_start = NULL, # Do not touch this unless you are expert
#'                     validation_start = NULL, # Do not touch this unless you are expert
#'                     n_forest = 2, # Number of forest models
#'                     n_trees = 30, # Number of trees per forest
#'                     random_forest = 1, # We want only 2 random forest
#'                     seed = 0,
#'                     objective = "binary:logistic",
#'                     eval_metric = Laurae::df_logloss,
#'                     multi_class = 2, # Modify this for multiclass problems)
#'                     verbose = TRUE)
#' 
#' # Create predictions
#' data_predictions <- model$preds
#' 
#' # Make real predictions
#' new_preds <- MGScanning_pred(model, data = agaricus_data_test)
#' 
#' # We can check whether we have equal predictions, it's all TRUE!
#' all.equal(model$preds, MGScanning_pred(model,
#'                                        agaricus_data_train,
#'                                        folds = folds))
#' 
#' # Example on fake pictures (matrices) and multiclass problem
#' 
#' # Generate fake images
#' new_data <- list(matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20))
#' 
#' # Generate fake labels
#' new_labels <- c(2, 1, 0, 2, 1, 0, 2, 1, 0, 0)
#' 
#' # Train a model (multiclass problem)
#' model <- MGScanning(data = new_data, # Training data
#'                     labels = new_labels, # Training labels
#'                     folds = list(1:3, 3:6, 7:10), # Folds for cross-validation
#'                     dimensions = 2,
#'                     depth = 10,
#'                     stride = 1,
#'                     nthread = 1, # Change this to use more threads
#'                     lr = 1, # Do not touch this unless you are expert
#'                     training_start = NULL, # Do not touch this unless you are expert
#'                     validation_start = NULL, # Do not touch this unless you are expert
#'                     n_forest = 2, # Number of forest models
#'                     n_trees = 10, # Number of trees per forest
#'                     random_forest = 1, # We want only 2 random forest
#'                     seed = 0,
#'                     objective = "multi:softprob",
#'                     eval_metric = Laurae::df_logloss,
#'                     multi_class = 3, # Modify this for multiclass problems)
#'                     verbose = TRUE)
#' 
#' # Matrix output is 10x600
#' dim(model$preds)
#' 
#' # We can check whether we have equal predictions, it's all TRUE!
#' all.equal(model$preds, MGScanning_pred(model,
#'                                        new_data,
#'                                        folds = list(1:3, 3:6, 7:10)))
#' 
#' # Real predictions on new data
#' new_data <- list(matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20),
#'                  matrix(rnorm(n = 400), ncol = 20, nrow = 20))
#' new_preds <- MGScanning_pred(model, data = new_data)
#' }
#' 
#' @export

MGScanning_pred <- function(model,
                            data,
                            folds = NULL,
                            dimensions = NULL,
                            multi_class = NULL,
                            data_start = NULL) {
  
  # Reverse engineer dimensions
  if (is.null(dimensions)) {
    if (is.null(model$dimensions)) {
      dimensions <- 2 - is.data.table(data) # data.table = 1, not data.table = 2
    } else {
      dimensions <- model$dimensions
    }
  }
  
  # Reverse engineer multi_class
  if (is.null(multi_class)) {
    if (is.null(model$multi_class)) {
      multi_class <- 2 # Attempt to guess...
    } else {
      multi_class <- model$multi_class
    }
  }
  
  if (dimensions == 1) {
    
    preds <- data.table(ID = 1:nrow(data))
    
    # Reverse engineer steps_perform
    steps_perform <- length(model$model)
    
    # Reverse engineer n_forest
    n_forest <- length(model$model[[1]]$features)
    
    for (i in 1:steps_perform) {
      
      # Prepare data
      temp_data <- Laurae::DTcolsample(data, model$model[[i]]$step)
      
      # Do predictions
      temp_preds <- CRTreeForest_pred(model = model$model[[i]],
                                      data = temp_data,
                                      folds = folds,
                                      prediction = FALSE,
                                      multi_class = multi_class,
                                      data_start = data_start,
                                      return_list = FALSE,
                                      work_dir = model$work_dir[[i]])
      
      if (multi_class > 2) {
        
        # Multiclass combination
        for (j in 1:(multi_class * n_forest)) {
          preds <- preds[, (paste0("Scan", sprintf(paste0("%0", floor(log10(steps_perform)) + 1, "d"), i), "_Forest", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), ((j - 1) %/% multi_class) + 1), "_Class", sprintf(paste0("%0", floor(log10(multi_class)) + 1, "d"), ((j - 1) %% multi_class) + 1))) := temp_preds[, j, with = FALSE]]
        }
        
      } else {
        
        # Not multiclass combination
        for (j in 1:n_forest) {
          preds <- preds[, (paste0("Scan", sprintf(paste0("%0", floor(log10(steps_perform)) + 1, "d"), i), "_Forest", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), j))) := temp_preds[, j, with = FALSE]]
        }
        
      }
      
    }
    
  } else {
    
    preds <- data.table(ID = 1:length(data))
    
    # Reverse engineer steps_perform
    steps_perform <- c(length(model$model), length(model$model[[1]]))
    
    # Reverse engineer n_forest
    n_forest <- length(model$model[[1]][[1]]$features)
    
    for (i in 1:steps_perform[1]) {
      
      for (j in 1:steps_perform[2]) {
        
        # Set step
        step <- model$model[[i]][[j]]$step
        
        # Prepare data
        temp_data <- data.table(matrix(unlist(lapply(data, function(x, step) {return(as.numeric(x[step[[1]], step[[2]]]))}, step = step)), ncol = ((max(step[[1]]) - min(step[[1]]) + 1) * (max(step[[2]]) - min(step[[2]]) + 1)), nrow = length(data), byrow = TRUE))
        
        # Do predictions
        temp_preds <- CRTreeForest_pred(model = model$model[[i]][[j]],
                                        data = temp_data,
                                        folds = folds,
                                        prediction = FALSE,
                                        multi_class = multi_class,
                                        data_start = data_start,
                                        return_list = FALSE,
                                        work_dir = model$work_dir[[i]][[j]])
        
        if (multi_class > 2) {
          
          # Multiclass combination
          for (k in 1:(multi_class * n_forest)) {
            preds <- preds[, (paste0("Scan_", sprintf(paste0("%0", floor(log10(steps_perform[2])) + 1, "d"), j), "x_", sprintf(paste0("%0", floor(log10(steps_perform[1])) + 1, "d"), i), "y_Forest", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), ((k - 1) %/% multi_class) + 1), "_Class", sprintf(paste0("%0", floor(log10(multi_class)) + 1, "d"), ((k - 1) %% multi_class) + 1))) := temp_preds[, k, with = FALSE]]
          }
          
        } else {
          
          # Not multiclass combination
          for (k in 1:n_forest) {
            preds <- preds[, (paste0("Scan_", sprintf(paste0("%0", floor(log10(steps_perform[2])) + 1, "d"), j), "x_", sprintf(paste0("%0", floor(log10(steps_perform[1])) + 1, "d"), i), "y_Forest", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), k))) := temp_preds[, k, with = FALSE]]
          }
          
        }
        
      }
      
    }
    
  }
  
  preds$ID <- NULL
  
  return(preds)
  
}
