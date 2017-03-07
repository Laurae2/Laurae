#' Cascade Forest implementation in R
#'
#' This function attempts to replicate Cascade Forest using xgboost. It performs Complete-Random Tree Forest in a Neural Network directed acrylic graph like in Neural Networks, but only for simple graphs (e.g use previous layer output data for next layer training each time). You can specify your learning objective using \code{objective} and the metric to check for using \code{eval_metric}. You can plug custom objectives instead of the objectives provided by \code{xgboost}. As with any uncalibrated machine learning methods, this method suffers uncalibrated outputs. Therefore, the usage of scale-dependent metrics is discouraged (please use scale-invariant metrics, such as Accuracy, AUC, R-squared, Spearman correlation...).
#' 
#' For implementation details of Cascade Forest / Complete-Random Tree Forest / Multi-Grained Scanning / Deep Forest, check this: \url{https://github.com/Microsoft/LightGBM/issues/331#issuecomment-283942390} by Laurae.
#' 
#' Cascade Forests tend to aim what Neural Networks are doing: architecturing the model in multiple layers. Cascade Forests abuse the stacking ensemble method to perform training on these layers. Using randomness of Random Forests and Complete-Random Tree Forests, a Cascade Forest aims to outperform simple Convolutional Neural Networks (CNNs). The computational cost, however, is massive and should be taken into account before learning a large (and potentially unlimited) Cascade Forest. Due to their nature and to stacking ensemble properties, Cascade Forests have a hard time to overfit themselves.
#' 
#' Putting a Cascade Forest on top a Multi-Grained Scanning model results in a gcForest
#' 
#' Laurae recommends using xgboost or LightGBM on top of gcForest or Cascade Forest. See the rationale here: \url{https://github.com/Microsoft/LightGBM/issues/331#issuecomment-284689795}.
#' 
#' @param training_data Type: data.table. The training data. Columns are added during training if \code{low_memory == TRUE}, so you may want to clean it up if you use \code{low_memory == TRUE} and interrupt training.
#' @param validation_data Type: data.table. The validation data to check for metric performance. Set to \code{NULL} if you want to use out of fold validation data instead of a custom validation data set. Columns are added during training if \code{low_memory == TRUE}, so you may want to clean it up if you use \code{low_memory == TRUE} and interrupt training.
#' @param training_labels Type: numeric vector. The training labels.
#' @param validation_labels Type: numeric vector. The validation labels.
#' @param folds Type: list. The folds as list for cross-validation.
#' @param boosting Type: logical. Whether to perform boosting or not for training. It may converge faster, but may overfit faster and therefore needs control via \code{cascade_lr}. Defaults to \code{FALSE}.
#' @param nthread Type: numeric. The number of threads using for multithreading. 1 means singlethread (uses only one core). Higher may mean faster training if the memory overhead is not too large. Defaults to \code{1}.
#' @param cascade_lr Type: numeric vector or numeric. The shrinkage affected to each tree per layer to avoid overfitting, for each layer. You may specify a vector to change the learning rate per layer, such as \code{c(0.4, 0.3, 0.2, 0.1, 0.05)} so you can perform boosting afterwards. Defaults to \code{1}.
#' @param training_start Type: numeric vector. The initial training prediction labels. Set to \code{NULL} if you do not know what you are doing. Defaults to \code{NULL}.
#' @param validation_start Type: numeric vector. The initial validation prediction labels. Set to \code{NULL} if you do not know what you are doing. Defaults to \code{NULL}.
#' @param cascade_forests Type: numeric vector (mandatory). The number of forest models per layer in the architecture to create for the Cascade Forest. Inputting 0 means it will take the latest number forever until it stops after convergence using \code{early_stopping}. For instance, to activate infinite training on the default parameters, use \code{c(rep(4, 5), 0)}. Defaults to \code{rep(4, 5)}.
#' @param cascade_trees Type: numeric vector or numeric. The number of trees per forest model per layer in the architecture to create for the Cascade Forest. You may specify a vector to change the learning rate per layer, such as \code{500} so you can perform boosting afterwards. Defaults to \code{1000}.
#' @param cascade_rf Type: numeric vector or numeric. The number of Random Forest model per layer in the architecture to create for the Cascade Forest. You may specify a vector to change the learning rate per layer, such as \code{c(1, 1, 2, 3, 5)} so you can perform boosting afterwards. Defaults to \code{2}.
#' @param cascade_seeds Type: numeric vector or numeric. Random seed for reproducibility per layer. Do not set it to a value which is identical throughout the architecture, you will train on the same features over and over otherwise! When using a single value as seed, it automatically adds 1 each time an advance in the layer is made. Defaults to \code{1:length(cascade_forests)}.
#' @param objective Type: character or function. The function which leads \code{boosting} loss. See \code{xgboost::xgb.train}. Defaults to \code{"reg:linear"}.
#' @param eval_metric Type: function. The function which evaluates \code{boosting} loss. Must take two arguments in the following order: \code{preds, labels} (they may be named in another way) and returns a metric. Defaults to \code{Laurae::df_rmse}.
#' @param multi_class Type: numeric. Defines the number of classes internally for whether you are doing multi class classification or not to use specific routines for multiclass problems when using \code{return_list == FALSE}. Defaults to \code{2}, which is for regression and binary classification.
#' @param early_stopping Type: numeric. Defines how many architecture layers without improvement to require before stopping early (therefore, you must remove 1 to that value - for instance, a stopping of 2 means it will stop after 3 failures to improve). 0 means instantly stop at the first failure for improvement. -1 means no stopping. Requires \code{validation_data} to be able to stop early. Defaults to \code{2}.
#' @param maximize Type: logical. Whether to maximize or not the loss evaluation metric. Defaults to \code{FALSE}.
#' @param verbose Type: logical. Whether to print training evaluation. Defaults to \code{TRUE}.
#' @param low_memory Type: logical. Whether to perform the data.table transformations in place to lower memory usage. Defaults to \code{FALSE}.
#' @param essentials Type: logical. Whether to store intermediary predictions or not. Set it to \code{TRUE} if you encounter memory issues. Defaults to \code{FALSE}.
#' @param garbage Type: logical. Whether to perform garbage collect regularly. Defaults to \code{FALSE}.
#' @param fail_safe Type: numeric. In case of infinite training (\code{cascade_forests}'s last value equal to 0), this limits the number of training iterations. Defaults to \code{65536}.
#' 
#' @return A data.table based on \code{target}.
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
#'                        cascade_seeds = 1:5, # Seed per layer
#'                        objective = "binary:logistic",
#'                        eval_metric = Laurae::df_logloss,
#'                        multi_class = 2, # Modify this for multiclass problems
#'                        early_stopping = 2, # stop after 2 bad combos of forests
#'                        maximize = FALSE, # not a maximization task
#'                        verbose = TRUE, # print information during training
#'                        low_memory = FALSE)
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
#'                        cascade_seeds = 1:5, # Seed per layer
#'                        objective = "multi:softprob",
#'                        eval_metric = Laurae::df_logloss,
#'                        multi_class = 3, # Modify this for multiclass problems
#'                        early_stopping = 2, # stop after 2 bad combos of forests
#'                        maximize = FALSE, # not a maximization task
#'                        verbose = TRUE, # print information during training
#'                        low_memory = FALSE)
#' }
#' 
#' @export

CascadeForest <- function(training_data,
                          validation_data,
                          training_labels,
                          validation_labels,
                          folds,
                          boosting = FALSE,
                          nthread = 1,
                          cascade_lr = 1,
                          training_start = NULL,
                          validation_start = NULL,
                          cascade_forests = rep(4, 5),
                          cascade_trees = 500,
                          cascade_rf = 2,
                          cascade_seeds = 1:length(cascade_forests),
                          objective = "reg:linear",
                          eval_metric = Laurae::df_rmse,
                          multi_class = FALSE,
                          early_stopping = 2,
                          maximize = FALSE,
                          verbose = TRUE,
                          low_memory = FALSE,
                          essentials = FALSE,
                          garbage = FALSE,
                          fail_safe = 65536) {
  
  model <- list()
  logger <- list()
  original_cols <- copy(ncol(training_data)) + 1 # Perform deep copy of columns
  patience <- early_stopping + 1 # for early_stopping
  
  # Check whether user wants infinite training
  if (cascade_forests[length(cascade_forests)] == 0) {
    cascade_infinite <- TRUE
    num_layers <- length(cascade_forests) - 1 # Define number of layers
  } else {
    cascade_infinite <- FALSE
    num_layers <- length(cascade_forests) # Define number of layers
  }
  
  # Check whether user has input a vector or a simple numeric for cascade_lr
  if (length(cascade_lr) == 1) {
    cascade_lr <- rep(cascade_lr, num_layers)
  }
  
  # Check whether user has input a vector or a simple numeric for cascade_trees
  if (length(cascade_trees) == 1) {
    cascade_trees <- rep(cascade_trees, num_layers)
  }
  
  # Check whether user has input a vector or a simple numeric for cascade_rf
  if (length(cascade_rf) == 1) {
    cascade_rf <- rep(cascade_rf, num_layers)
  }
  
  # Check whether user has input a vector or a simple numeric for cascade_seeds
  if (length(cascade_seeds) == 1) {
    cascade_seeds <- cascade_seeds:(cascade_seeds + num_layers - 1)
  }
  
  # Are we using the low_memory implementation? (does not copy data.table in place)
  if (low_memory == TRUE) {
    
    # Low-memory training
    
    # Create initial frames
    train_data <- training_data
    valid_data <- validation_data
    
    # Perform architecture training
    for (i in 1:fail_safe) {
      
      # Check next layer for infinite training
      if (cascade_infinite) {
        # Check if it is the last layer
        if (length(cascade_forests) == (i + 1)) {
          cascade_forests[i + 1] <- cascade_forests[i]
          cascade_forests[i + 2] <- 0
          cascade_lr[i + 1] <- cascade_lr[i]
          cascade_trees[i + 1] <- cascade_trees[i]
          cascade_rf[i + 1] <- cascade_rf[i]
          cascade_seeds[i + 1] <- cascade_seeds[i] + 1
        }
        # Continuously add layers
        num_layers <- i + 2
      }
      
      # Train model
      model[[i]] <- CRTreeForest(training_data = train_data,
                                 validation_data = valid_data,
                                 training_labels = training_labels,
                                 validation_labels = validation_labels,
                                 folds = folds,
                                 nthread = nthread,
                                 lr = cascade_lr[i],
                                 training_start = training_start,
                                 validation_start = validation_start,
                                 n_forest = cascade_forests[i],
                                 n_trees = cascade_trees[i],
                                 random_forest = cascade_rf[i],
                                 seed = cascade_seeds[i],
                                 objective = objective,
                                 eval_metric = eval_metric,
                                 return_list = FALSE,
                                 multi_class = multi_class,
                                 verbose = ifelse(verbose == FALSE, "", paste0("Layer ", sprintf(paste0("%0", floor(log10(num_layers - (2 * cascade_infinite))) + 1, "d"), i), ", ")),
                                 garbage = garbage)
      
      logger[[i]] <- model[[i]]$logger[[2]]
      
      # Check for early stopping
      if ((early_stopping > -1) & (i > 1)) {
        
        # Check whether to maximize or not metric
        if (maximize == FALSE) {
          
          # Remove or reset patience
          if (logger[[i]] >= logger[[i - 1]]) {
            patience <- patience - 1 - ((patience - 1) == early_stopping)
          } else {
            patience <- early_stopping + 1
          }
          
        } else {
          
          # Remove or reset patience
          if (logger[[i]] <= logger[[i - 1]]) {
            patience <- patience - 1 - ((patience - 1) == early_stopping)
          } else {
            patience <- early_stopping + 1
          }
          
        }
        
      }
      
      # Check if it is the last layer
      if ((i == num_layers) | (i == fail_safe) | (patience < 0)) {
        
        if (patience < 0) {
          best_iteration <- i - early_stopping - 1
        } else {
          best_iteration <- num_layers
        }
        
        train_means <- copy(model[[i]]$train_means)
        valid_means <- copy(model[[i]]$valid_means)
        
        # Setup names
        names(model) <- paste0("Layer_", sprintf(paste0("%0", floor(log10(num_layers)) + 1, "d"), 1:i))
        
        # Restore original data
        DTcolsample(training_data, kept = original_cols:copy(ncol(training_data)), remove = TRUE, low_mem = TRUE)
        
        # Check for validation data
        if (!is.null(validation_data)) {
          
          # Restore original data
          DTcolsample(validation_data, kept = original_cols:copy(ncol(validation_data)), remove = TRUE, low_mem = TRUE)
          
        }
        
        # Check for essential needs
        if (essentials) {
          model[[i]]$train_preds <- NULL
          model[[i]]$valid_preds <- NULL
          model[[i]]$train_means <- NULL
          model[[i]]$valid_means <- NULL
        }
        
        # Return to user
        return(list(model = model,
                    logger = logger,
                    train_means = train_means,
                    valid_means = valid_means,
                    multi_class = multi_class,
                    folds = folds,
                    best_iteration = best_iteration))
        
      } else {
        
        # Not the last layer, therefore we bind predictions, or overwrite them
        training_data <- Laurae::DTcbind(training_data, model[[i]]$train_preds) # Routine overwrites if column names are identical
        training_start <- if (boosting) {rowMeans(model[[i]]$train_preds)} else {NULL}
        validation_data <- Laurae::DTcbind(validation_data, model[[i]]$valid_preds) # Routine overwrites if column names are identical
        validation_start <- if (boosting) {rowMeans(model[[i]]$valid_preds)} else {NULL}
        
        # Check for essential needs
        if (essentials) {
          model[[i]]$train_preds <- NULL
          model[[i]]$valid_preds <- NULL
          model[[i]]$train_means <- NULL
          model[[i]]$valid_means <- NULL
        }
        
        if (garbage) {gc(verbose = FALSE)}
        
      }
      
    }
    
  } else {
    
    # Not low-memory training
    
    # Copy in-place
    train_data <- copy(training_data)
    valid_data <- copy(validation_data)
    
    # Perform architecture training
    for (i in 1:fail_safe) {
      
      # Check next layer for infinite training
      if (cascade_infinite) {
        # Check if it is the last layer
        if (length(cascade_forests) == (i + 1)) {
          cascade_forests[i + 1] <- cascade_forests[i]
          cascade_forests[i + 2] <- 0
          cascade_lr[i + 1] <- cascade_lr[i]
          cascade_trees[i + 1] <- cascade_trees[i]
          cascade_rf[i + 1] <- cascade_rf[i]
          cascade_seeds[i + 1] <- cascade_seeds[i] + 1
        }
        # Continuously add layers
        num_layers <- i + 2
      }
      
      # Train model
      model[[i]] <- CRTreeForest(training_data = train_data,
                                 validation_data = valid_data,
                                 training_labels = training_labels,
                                 validation_labels = validation_labels,
                                 folds = folds,
                                 nthread = nthread,
                                 lr = cascade_lr[i],
                                 training_start = training_start,
                                 validation_start = validation_start,
                                 n_forest = cascade_forests[i],
                                 n_trees = cascade_trees[i],
                                 random_forest = cascade_rf[i],
                                 seed = cascade_seeds[i],
                                 objective = objective,
                                 eval_metric = eval_metric,
                                 return_list = FALSE,
                                 multi_class = multi_class,
                                 verbose = ifelse(verbose == FALSE, "", paste0("Layer ", sprintf(paste0("%0", floor(log10(num_layers - (2 * cascade_infinite))) + 1, "d"), i), ", ")),
                                 garbage = garbage)
      
      logger[[i]] <- model[[i]]$logger[[2]]
      
      # Check for early stopping
      if ((early_stopping > -1) & (i > 1)) {
        
        # Check whether to maximize or not metric
        if (maximize == FALSE) {
          
          # Remove or reset patience
          if (logger[[i]] >= logger[[i - 1]]) {
            patience <- patience - 1 - ((patience - 1) == early_stopping)
          } else {
            patience <- early_stopping + 1
          }
          
        } else {
          
          # Remove or reset patience
          if (logger[[i]] <= logger[[i - 1]]) {
            patience <- patience - 1 - ((patience - 1) == early_stopping)
          } else {
            patience <- early_stopping + 1
          }
          
        }
        
      }
      
      # Check if it is the last layer
      if ((i == num_layers) | (i == fail_safe) | (patience < 0)) {
        
        if (patience < 0) {
          best_iteration <- i - early_stopping - 1
        } else {
          best_iteration <- num_layers
        }
        
        train_means <- copy(model[[i]]$train_means)
        valid_means <- copy(model[[i]]$valid_means)
        
        # Setup names
        names(model) <- paste0("Layer_", sprintf(paste0("%0", floor(log10(num_layers)) + 1, "d"), 1:i))
        names(logger) <- paste0("Layer_", sprintf(paste0("%0", floor(log10(num_layers)) + 1, "d"), 1:i))
        
        # Check for essential needs
        if (essentials) {
          model[[i]]$train_preds <- NULL
          model[[i]]$valid_preds <- NULL
          model[[i]]$train_means <- NULL
          model[[i]]$valid_means <- NULL
        }
        
        # Return to user
        return(list(model = model,
                    logger = logger,
                    train_means = train_means,
                    valid_means = valid_means,
                    multi_class = multi_class,
                    folds = folds,
                    best_iteration = best_iteration))
        
      } else {
        
        # Not the last layer, therefore we bind predictions
        train_data <- Laurae::DTcbind(training_data, model[[i]]$train_preds)
        training_start <- if (boosting) {rowMeans(model[[i]]$train_preds)} else {NULL}
        valid_data <- Laurae::DTcbind(validation_data, model[[i]]$valid_preds)
        validation_start <- if (boosting) {rowMeans(model[[i]]$valid_preds)} else {NULL}
        
        # Check for essential needs
        if (essentials) {
          model[[i]]$train_preds <- NULL
          model[[i]]$valid_preds <- NULL
          model[[i]]$train_means <- NULL
          model[[i]]$valid_means <- NULL
        }
        
        if (garbage) {gc(verbose = FALSE)}
        
      }
      
    }
    
  }
  
}
