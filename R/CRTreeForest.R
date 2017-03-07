#' Complete-Random Tree Forest implementation in R
#'
#' This function attempts to replicate Complete-Random Tree Forests using xgboost. It performs Random Forest \code{n_forest} times using \code{n_trees} trees. You can specify your learning objective using \code{objective} and the metric to check for using \code{eval_metric}. You can plug custom objectives instead of the objectives provided by \code{xgboost}. As with any uncalibrated machine learning methods, this method suffers uncalibrated outputs. Therefore, the usage of scale-dependent metrics is discouraged (please use scale-invariant metrics, such as Accuracy, AUC, R-squared, Spearman correlation...).
#' 
#' For implementation details of Cascade Forest / Complete-Random Tree Forest / Multi-Grained Scanning / Deep Forest, check this: \url{https://github.com/Microsoft/LightGBM/issues/331#issuecomment-283942390} by Laurae.
#' 
#' Actually, this function creates a layer of a Cascade Forest. That layer is comprised of two possible elements: Complete-Random Tree Forests (using PFO mode: Probability Averaging + Full Height + Original training samples) and Random Forests. You may choose between them.
#' 
#' Complete-Random Tree Forests in PFO mode are the best random learners inside the Complete-Random Tree Forest families (at least 50% higher winrate against other families, including Random Forest). The major issue is their randomness which lowers their performance until they are fully extended for maximum performance: it takes a long time to train them properly until the features are so obvious they learn nearly instantly in one run of training. Therefore, they are extremely prone to underfitting, and a \code{CascadeForest} should be used to improve their performance combined with one or multiple Random Forest.
#' 
#' Laurae recommends using xgboost or LightGBM on top of gcForest or Cascade Forest. See the rationale here: \url{https://github.com/Microsoft/LightGBM/issues/331#issuecomment-284689795}.
#' 
#' @param training_data Type: data.table. The training data.
#' @param validation_data Type: data.table. The validation data with labels to check for metric performance. Set to \code{NULL} if you want to use out of fold validation data instead of a custom validation data set.
#' @param training_labels Type: numeric vector. The training labels.
#' @param validation_labels Type: numeric vector. The validation labels.
#' @param folds Type: list. The folds as list for cross-validation.
#' @param nthread Type: numeric. The number of threads using for multithreading. 1 means singlethread (uses only one core). Higher may mean faster training if the memory overhead is not too large. Defaults to \code{1}.
#' @param lr Type: numeric. The shrinkage affected to each tree to avoid overfitting. Defaults to \code{1}, which means no adjustment.
#' @param training_start Type: numeric vector. The initial training prediction labels. Set to \code{NULL} if you do not know what you are doing. Defaults to \code{NULL}.
#' @param validation_start Type: numeric vector. The initial validation prediction labels. Set to \code{NULL} if you do not know what you are doing. Defaults to \code{NULL}.
#' @param n_forest Type: numeric. The number of forest models to create for the Complete-Random Tree Forest. Defaults to \code{5}.
#' @param n_trees Type: numeric. The number of trees per forest model to create for the Complete-Random Tree Forest. Defaults to \code{1000}.
#' @param random_forest Type: numeric. The number of Random Forest in the forest. Defaults to \code{0}.
#' @param seed Type: numeric. Random seed for reproducibility. Defaults to \code{0}.
#' @param objective Type: character or function. The function which leads \code{boosting} loss. See \code{xgboost::xgb.train}. Defaults to \code{"reg:linear"}.
#' @param eval_metric Type: function. The function which evaluates \code{boosting} loss. Must take two arguments in the following order: \code{preds, labels} (they may be named in another way) and returns a metric. Defaults to \code{Laurae::df_rmse}.
#' @param return_list Type: logical. Whether lists should be returned instead of concatenated frames for predictions. Defaults to \code{TRUE}.
#' @param multi_class Type: numeric. Defines the number of classes internally for whether you are doing multi class classification or not to use specific routines for multiclass problems when using \code{return_list == FALSE}. Defaults to \code{2}, which is for regression and binary classification.
#' @param verbose Type: character. Whether to print for training evaluation. Use \code{""} for no printing (double quotes without space between quotes). Defaults to \code{" "} (double quotes with space between quotes.
#' @param garbage Type: logical. Whether to perform garbage collect regularly. Defaults to \code{FALSE}.
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
#' }
#' 
#' @export

CRTreeForest <- function(training_data,
                         validation_data,
                         training_labels,
                         validation_labels,
                         folds,
                         nthread = 1,
                         lr = 1,
                         training_start = NULL,
                         validation_start = NULL,
                         n_forest = 5,
                         n_trees = 1000,
                         random_forest = 0,
                         seed = 0,
                         objective = "reg:linear",
                         eval_metric = Laurae::df_rmse,
                         return_list = TRUE,
                         multi_class = 2,
                         verbose = " ",
                         garbage = FALSE) {
  
  model <- list()
  train_preds <- list()
  valid_preds <- list()
  logger <- list()
  logger[[1]] <- list()
  logger[[2]] <- numeric(n_forest)
  features_used <- list()
  
  premade_folds <- !(is.null(validation_data))
  
  # Setup train_means / valid_means
  if (multi_class > 2) {
    
    # Setup train
    train_means <- data.table(matrix(rep(0, nrow(training_data) * multi_class), nrow = nrow(training_data), ncol = multi_class))
    
    # Are we using premade folds?
    if (!premade_folds) {
      
      # Using training data
      valid_means <- data.table(matrix(rep(0, nrow(training_data) * multi_class), nrow = nrow(training_data), ncol = multi_class))
      
    } else {
      
      # Using validation data
      valid_means <- data.table(matrix(rep(0, nrow(validation_data) * multi_class), nrow = nrow(validation_data), ncol = multi_class))
      
    }
    
    # Name columns
    colnames(train_means) <- paste0("Label_", sprintf(paste0("%0", floor(log10(multi_class)) + 1, "d"), 1:multi_class))
    colnames(valid_means) <- paste0("Label_", sprintf(paste0("%0", floor(log10(multi_class)) + 1, "d"), 1:multi_class))
    
  } else {
    
    # Setup train
    train_means <- numeric(nrow(training_data))
    
    # Are we using premade folds?
    if (!premade_folds) {
      
      # Using training data
      valid_means <- numeric(nrow(training_data))
      
    } else {
      
      # Using validation data
      valid_means <- numeric(nrow(validation_data))
      
    }
    
  }
  
  # Loop through the forest
  for (i in 1:n_forest) {
    
    # Check for Random Forest
    if (i <= random_forest) {
      
      # Setup parameters for Random Forest
      column_sampling_tree <- 1
      column_sampling_level <- ceiling(sqrt(ncol(training_data))) / ncol(training_data)
      row_sampling <- 0.632
      
      features_used[[i]] <- 1:ncol(training_data)
      
    } else {
      
      # Setup parameters not for Random Forest
      column_sampling_tree <- ceiling(sqrt(ncol(training_data)))
      column_sampling_level <- 1/(column_sampling_tree)
      row_sampling <- 1
      
      # Sample features
      set.seed(seed + i)
      features_used[[i]] <- sample(1:ncol(training_data), column_sampling_tree)
      
    }
    
    # Are we doing multiclass?
    if (multi_class > 2) {
      train_preds[[i]] <- data.table(matrix(rep(0, nrow(training_data) * multi_class), nrow = nrow(training_data), ncol = multi_class))
      if (!premade_folds) {
        valid_preds[[i]] <- data.table(matrix(rep(0, nrow(training_data) * multi_class), nrow = nrow(training_data), ncol = multi_class))
      } else {
        valid_preds[[i]] <- data.table(matrix(rep(0, nrow(validation_data) * multi_class), nrow = nrow(validation_data), ncol = multi_class))
      }
    } else {
      train_preds[[i]] <- numeric(nrow(training_data))
      if (!premade_folds) {
        valid_preds[[i]] <- numeric(nrow(training_data))
      } else {
        valid_preds[[i]] <- numeric(nrow(validation_data))
      }
    }
    
    # More initialization
    model[[i]] <- list()
    logger[[1]][[i]] <- numeric(length(folds))
    
    for (j in 1:length(folds)) {
      
      # Split data
      to_train_data <- Laurae::DTcolsample(training_data, kept = features_used[[i]])
      train_data <- Laurae::DTsubsample(to_train_data, kept = (1:nrow(training_data))[-folds[[j]]], remove = FALSE)
      test_data <- Laurae::DTsubsample(to_train_data, kept = folds[[j]], remove = FALSE)
      train_data <- xgb.DMatrix(data = Laurae::DT2mat(train_data), label = training_labels[(1:nrow(training_data))[-folds[[j]]]], base_margin = training_start[(1:nrow(training_data))[-folds[[j]]]])
      test_data <- xgb.DMatrix(data = Laurae::DT2mat(test_data), label = training_labels[folds[[j]]], base_margin = training_start[folds[[j]]])
      
      if (premade_folds) {
        to_validate_data <- Laurae::DTcolsample(validation_data, kept = features_used[[i]])
        validate_data <- xgb.DMatrix(data = Laurae::DT2mat(to_validate_data), label = validation_labels, base_margin = validation_start)
      } else {
        validate_data <- test_data
      }
      
      if (garbage) {gc(verbose = FALSE)}
      
      # Train model while checking for multiclass routines
      if (multi_class > 2) {
        
        # Multiclass training
        set.seed(seed + i)
        model[[i]][[j]] <- xgb.train(params = list(booster = "gbtree",
                                                   eta = lr,
                                                   max_depth = 99999,
                                                   max_leaves = 99999,
                                                   colsample_bytree = 1,
                                                   colsample_bylevel = column_sampling_level,
                                                   subsample = row_sampling,
                                                   num_parallel_tree = n_trees),
                                     nthread = nthread,
                                     data = train_data,
                                     nrounds = 1,
                                     verbose = 0,
                                     watchlist = list(test = validate_data),
                                     objective = objective,
                                     num_class = multi_class)
        
        if (garbage) {gc(verbose = FALSE)}
        
        # Predict out of fold predictions
        train_preds[[i]][folds[[j]]] <- data.table(predict(model[[i]][[j]], test_data, reshape = TRUE))
        
        # Check for validation
        if (!premade_folds) {
          valid_preds[[i]][folds[[j]]] <- train_preds[[i]][folds[[j]]]
          logger[[1]][[i]][j] <- eval_metric(valid_preds[[i]][folds[[j]]], training_labels[folds[[j]]])
        } else {
          temp_preds <- data.table(predict(model[[i]][[j]], validate_data, reshape = TRUE))
          logger[[1]][[i]][j] <- eval_metric(temp_preds, validation_labels)
          valid_preds[[i]] <- (temp_preds / length(folds)) + valid_preds[[i]]
        }
        
      } else {
        
        # Binary class or regression training
        set.seed(seed + i)
        model[[i]][[j]] <- xgb.train(params = list(booster = "gbtree",
                                                   eta = lr,
                                                   max_depth = 99999,
                                                   max_leaves = 99999,
                                                   colsample_bytree = 1,
                                                   colsample_bylevel = column_sampling_level,
                                                   subsample = row_sampling,
                                                   num_parallel_tree = n_trees),
                                     nthread = nthread,
                                     data = train_data,
                                     nrounds = 1,
                                     verbose = 0,
                                     watchlist = list(test = validate_data),
                                     objective = objective)
        
        if (garbage) {gc(verbose = FALSE)}
        
        # Predict out of fold predictions
        train_preds[[i]][folds[[j]]] <- predict(model[[i]][[j]], test_data, reshape = TRUE)
        
        # Check for validation
        if (!premade_folds) {
          valid_preds[[i]][folds[[j]]] <- train_preds[[i]][folds[[j]]]
          logger[[1]][[i]][j] <- eval_metric(valid_preds[[i]][folds[[j]]], training_labels[folds[[j]]])
        } else {
          temp_preds <- predict(model[[i]][[j]], validate_data, reshape = TRUE)
          logger[[1]][[i]][j] <- eval_metric(temp_preds, validation_labels)
          valid_preds[[i]] <- (temp_preds / length(folds)) + valid_preds[[i]]
        }
        
      }
      
      # Clear up old matrices
      rm(train_data, test_data, validate_data)
      
      if (garbage) {gc(verbose = FALSE)}
      
    }
    
    # Name elements
    names(model[[i]]) <- paste0("Fold_", sprintf(paste0("%0", floor(log10(length(folds))) + 1, "d"), 1:length(folds)))
    
    # Add to aggregate data
    train_means <- train_means + (train_preds[[i]] / n_forest)
    valid_means <- valid_means + (valid_preds[[i]] / n_forest)
    
    # Print cross-validation
    if (!(verbose == "")) {
      cat(verbose, "Forest ", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), i), ": ", sprintf("%08.06f", mean(logger[[1]][[i]])), "+", sprintf("%08.06f", sd(logger[[1]][[i]])), "\n", sep = "")
    }
    
  }
  
  # paste0("Fold_", sprintf(paste0("%0", floor(log10(length(folds))) + 1, "d"), 1:length(folds)))
  names(model) <- paste0("Forest_", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), 1:n_forest))
  names(logger[[1]]) <- paste0("Log_", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), 1:n_forest))
  names(train_preds) <- paste0("Forest_", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), 1:n_forest))
  
  # Parse logger
  logger[[1]] <- Laurae::cbindlist(logger[[1]])
  
  # Create new logger
  logger[[2]] <- eval_metric(valid_means, if (!premade_folds) {training_labels} else {validation_labels})
  
  # Print average forest
  if (!(verbose == "")) {
    cat(verbose, "Average Forest: ", sprintf("%08.06f", logger[[2]]), "\n", sep = "")
  }
  
  # Do we want data.tables instead of lists?
  if (return_list == FALSE) {
    
    # Is the problem a multiclass problem? (exports list of data.table instead of list of vector)
    if (multi_class > 2) {
      
      # Rename each column
      for (i in 1:n_forest) {
        colnames(train_preds[[i]]) <- paste0("Forest_", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), i), "_", sprintf(paste0("%0", floor(log10(ncol(train_preds[[i]]))) + 1, "d"), 1:ncol(train_preds[[i]])))
      }
      
      train_dt <- train_preds[[1]]
      
      # Do we have more than one model in forest?
      if (n_forest > 1) {
        
        # Attempt to bind each data.table together
        for (i in 2:n_forest) {
          train_dt <- Laurae::DTcbind(train_dt, train_preds[[i]])
        }
      }
      
      # Deeply overwrite original table
      train_preds <- copy(train_dt)
      
    } else {
      
      # Only vectors, so we can cbindlist directly
      train_preds <- Laurae::cbindlist(train_preds)
      
    }
    
    if (garbage) {gc(verbose = FALSE)}
    
  }
  
  # Do the same for validation
  
  # Prepame accordingly
  names(valid_preds) <- paste0("Forest_", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), 1:n_forest))
  
  # Do we want data.tables instead of lists?
  if (return_list == FALSE) {
    
    # Is the problem a multiclass problem? (exports list of data.table instead of list of vector)
    if (multi_class > 2) {
      
      # Rename each column
      for (i in 1:n_forest) {
        colnames(valid_preds[[i]]) <- paste0("Forest_", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), i), "_", sprintf(paste0("%0", floor(log10(ncol(valid_preds[[i]]))) + 1, "d"), 1:ncol(valid_preds[[i]])))
      }
      
      valid_dt <- valid_preds[[1]]
      
      # Do we have more than one model in forest?
      if (n_forest > 1) {
        
        # Attempt to bind each data.table together
        for (i in 2:n_forest) {
          valid_dt <- Laurae::DTcbind(valid_dt, valid_preds[[i]])
        }
      }
      
      # Deeply overwrite original table
      valid_preds <- copy(valid_dt)
      
    } else {
      
      # Only vectors, so we can cbindlist directly
      valid_preds <- Laurae::cbindlist(valid_preds)
      
    }
    
    if (garbage) {gc(verbose = FALSE)}
    
  }
  
  # Return data with validation
  return(list(model = model,
              logger = logger,
              train_means = train_means,
              valid_means = valid_means,
              train_preds = train_preds,
              valid_preds = valid_preds,
              features = features_used,
              multi_class = multi_class,
              folds = folds))
  
}
