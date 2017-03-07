#' Multi-Grained Scanning implementation in R
#'
#' This function attempts to replicate Multi-Grained Scanning using xgboost. It performs Random Forest \code{n_forest} times using \code{n_trees} trees on your data using a sliding window to create features. You can specify your learning objective using \code{objective} and the metric to check for using \code{eval_metric}. You can plug custom objectives instead of the objectives provided by \code{xgboost}. As with any uncalibrated machine learning methods, this method suffers uncalibrated outputs. Therefore, the usage of scale-dependent metrics is discouraged (please use scale-invariant metrics, such as Accuracy, AUC, R-squared, Spearman correlation...).
#' 
#' For implementation details of Cascade Forest / Complete-Random Tree Forest / Multi-Grained Scanning / Deep Forest, check this: \url{https://github.com/Microsoft/LightGBM/issues/331#issuecomment-283942390} by Laurae.
#' 
#' Multi-Grained Scanning attempts to perform a sort of specialized convolution using the stacking ensemble method from Cascade Forests. They do so by using one layer of Cascade Forest, which can be trained manually using \code{CRTreeForest}. The \code{depth} defines how wide the feature selection is done on each iteration of training. The window slides down/right by \code{stride} every time the training finishes to attempt to learn something else. A low \code{stride} allows fine-grained training, while a larger \code{stride} will attempt to go fast over the data. This could be said the same about \code{depth}, where a small value increases randomness but a large value decreases it (if a powerful feature is present in nearly all the trainings, then you are basically screwed up).
#' 
#' Using Multi-Grained Scanning before a Cascade Forest results in a gcForest.
#' 
#' Laurae recommends using xgboost or LightGBM on top of gcForest or Cascade Forest. See the rationale here: \url{https://github.com/Microsoft/LightGBM/issues/331#issuecomment-284689795}.
#' 
#' @param data Type: data.table (\code{dimensions == 1}) or list of matrices (\code{dimensions == 2}). The training data.
#' @param labels Type: numeric vector. The training labels.
#' @param folds Type: list. The folds as list for cross-validation.
#' @param dimensions Type: numeric. The dimensions of the data. Only supported is \code{1} for matrix format, and \code{2} for list of matrices. Defaults to \code{1}.
#' @param depth Type: numeric. The size of the sliding window applied. Use a vector of size 2 when using two dimensions (row, col). Do not make it larger than \code{ncol(data)} when \code{dimensions == 1}, or when \code{dimensions == 2} the \code{depth} must be smaller than the height and width of each matrix. Defaults to \code{2}.
#' @param stride Type: numeric. The stride (sliding steps) applied to each sliding window. Use a vector of size 2 when using two dimensions (row, col). Defaults to \code{1}.
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
#' @param multi_class Type: logical. Defines internally whether you are doing multi class classification or not to use specific routines for multiclass problems when using \code{return_list == FALSE}. Defaults to \code{FALSE}.
#' @param verbose Type: character. Whether to print for training evaluation. Use \code{""} for no printing (double quotes without space between quotes). Defaults to \code{" "} (double quotes with space between quotes.
#' @param garbage Type: logical. Whether to perform garbage collect regularly. Defaults to \code{FALSE}.
#' @param work_dir Type: character, allowing concatenation with another character text (ex: "dev/tools/save_in_this_folder/" = add slash, or "dev/tools/save_here/prefix_" = don't add slash). The working directory to store models. If you provide a working directory, the models will be saved inside that directory (and all other models will get wiped if they are under the same names). It will lower severely the memory usage as the models will not be saved anymore in memory. Combined with \code{garbage == TRUE}, you achieve the lowest possible memory usage in this Deep Forest implementation. Defaults to \code{NULL}, which means store models in memory.
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
#' }
#' 
#' @export

MGScanning <- function(data,
                       labels,
                       folds,
                       dimensions = 1,
                       depth = 10,
                       stride = 1,
                       nthread = 1,
                       lr = 1,
                       training_start = NULL,
                       validation_start = NULL,
                       n_forest = 2,
                       n_trees = 30,
                       random_forest = 1,
                       seed = 0,
                       objective = "reg:linear",
                       eval_metric = Laurae::df_rmse,
                       multi_class = 2,
                       verbose = TRUE,
                       garbage = FALSE,
                       work_dir = NULL) {
  
  model <- list()
  model_path <- list()
  
  out_of_memory <- !is.null(work_dir)
  
  if (dimensions == 1) {
    
    # One-Dimensional Scan
    
    steps_perform <- ceiling((copy(ncol(data)) - depth) / stride)
    step <- 1:depth
    preds <- data.table(ID = 1:nrow(data))
    
    # Slide observations
    for (i in 1:steps_perform) {
      
      # Create training data
      training_data <- Laurae::DTcolsample(data, step)
      
      # Train model
      model[[i]] <- CRTreeForest(training_data = training_data,
                                 validation_data = NULL,
                                 training_labels = labels,
                                 validation_labels = NULL,
                                 folds = folds,
                                 nthread = nthread,
                                 lr = lr,
                                 training_start = training_start,
                                 validation_start = validation_start,
                                 n_forest = n_forest,
                                 n_trees = n_trees,
                                 random_forest = 1,
                                 seed = seed,
                                 objective = objective,
                                 eval_metric = eval_metric,
                                 return_list = FALSE,
                                 multi_class = multi_class,
                                 verbose = ifelse(verbose == TRUE, paste0("Scan ", sprintf(paste0("%0", floor(log10(steps_perform)) + 1, "d"), i), "/", steps_perform, ", "), ""),
                                 garbage = garbage,
                                 work_dir = if (out_of_memory) {paste0(work_dir, paste0("Scan", sprintf(paste0("%0", floor(log10(steps_perform)) + 1, "d"), i), "_"))} else {NULL})
      
      model[[i]]$step <- step
      model_path[[i]] <- model[[i]]$work_dir
      
      if (multi_class > 2) {
        
        # Multiclass combination
        for (j in 1:(multi_class * n_forest)) {
          preds <- preds[, (paste0("Scan", sprintf(paste0("%0", floor(log10(steps_perform)) + 1, "d"), i), "_Forest", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), ((j - 1) %/% multi_class) + 1), "_Class", sprintf(paste0("%0", floor(log10(multi_class)) + 1, "d"), ((j - 1) %% multi_class) + 1))) := model[[i]]$train_preds[, j, with = FALSE]]
        }
        
        if (garbage) {gc(verbose = FALSE)}
        
      } else {
        
        # Not multiclass combination
        for (j in 1:n_forest) {
          preds <- preds[, (paste0("Scan", sprintf(paste0("%0", floor(log10(steps_perform)) + 1, "d"), i), "_Forest", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), j))) := model[[i]]$train_preds[, j, with = FALSE]]
        }
        
        if (garbage) {gc(verbose = FALSE)}
        
      }
      
      step <- (min(step) + stride):min(max(step) + stride, ncol(data))
      
    }
    
    preds$ID <- NULL
    
    return(list(model = model,
                preds = preds,
                dimensions = dimensions,
                multi_class = multi_class,
                folds = folds,
                work_dir = model_path))
    
  } else {
    
    # Two-dimensional Scan
    
    steps_perform <- c(ceiling((copy(nrow(data[[1]])) - depth[1]) / stride[1]), ceiling((copy(ncol(data[[1]])) - depth[length(depth)]) / stride[length(stride)]))
    step <- list(1:depth[1], 1:depth[length(depth)])
    preds <- data.table(ID = 1:length(data))
    
    # Slide vertically
    for (i in 1:steps_perform[1]) {
      
      model[[i]] <- list()
      model_path[[i]] <- list()
      
      # Reset steps
      step[[2]] <- 1:depth[length(depth)]
      
      # Slide horizontally
      for (j in 1:steps_perform[2]) {
        
        # Create training data
        training_data <- data.table(matrix(unlist(lapply(data, function(x, step) {return(as.numeric(x[step[[1]], step[[2]]]))}, step = step)), ncol = ((max(step[[1]]) - min(step[[1]]) + 1) * (max(step[[2]]) - min(step[[2]]) + 1)), nrow = length(data), byrow = TRUE))
        
        # Train model
        model[[i]][[j]] <- CRTreeForest(training_data = training_data,
                                        validation_data = NULL,
                                        training_labels = labels,
                                        validation_labels = NULL,
                                        folds = folds,
                                        nthread = nthread,
                                        lr = lr,
                                        training_start = training_start,
                                        validation_start = validation_start,
                                        n_forest = n_forest,
                                        n_trees = n_trees,
                                        random_forest = 1,
                                        seed = seed,
                                        objective = objective,
                                        eval_metric = eval_metric,
                                        return_list = FALSE,
                                        multi_class = multi_class,
                                        verbose = ifelse(verbose == TRUE, paste0("Scan x=", sprintf(paste0("%0", floor(log10(steps_perform[1])) + 1, "d"), i), "/", steps_perform[1], ", y=", sprintf(paste0("%0", floor(log10(steps_perform[2])) + 1, "d"), j), "/", steps_perform[2], ", "), ""),
                                        garbage = garbage,
                                        work_dir = if (out_of_memory) {paste0(work_dir, paste0("Scan_x", sprintf(paste0("%0", floor(log10(steps_perform[1])) + 1, "d"), i), "_y", , sprintf(paste0("%0", floor(log10(steps_perform[2])) + 1, "d"), j), "_"))} else {NULL})
        
        model[[i]][[j]]$step <- step
        model_path[[i]][[j]] <- model[[i]][[j]]$work_dir
        
        if (multi_class > 2) {
          
          # Multiclass combination
          for (k in 1:(multi_class * n_forest)) {
            preds <- preds[, (paste0("Scan_", sprintf(paste0("%0", floor(log10(steps_perform[2])) + 1, "d"), j), "x_", sprintf(paste0("%0", floor(log10(steps_perform[1])) + 1, "d"), i), "y_Class", sprintf(paste0("%0", floor(log10(multi_class)) + 1, "d"), j), "_Forest", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), ((k - 1) %/% multi_class) + 1), "_Class", sprintf(paste0("%0", floor(log10(multi_class)) + 1, "d"), ((k - 1) %% multi_class) + 1))) := model[[i]][[j]]$train_preds[, k, with = FALSE]]
          }
          
          if (garbage) {gc(verbose = FALSE)}
          
        } else {
          
          # Not multiclass combination
          for (k in 1:n_forest) {
            preds <- preds[, (paste0("Scan_", sprintf(paste0("%0", floor(log10(steps_perform[2])) + 1, "d"), j), "x_", sprintf(paste0("%0", floor(log10(steps_perform[1])) + 1, "d"), i), "y_Forest", sprintf(paste0("%0", floor(log10(n_forest)) + 1, "d"), k))) := model[[i]][[j]]$train_preds[, k, with = FALSE]]
          }
          
          if (garbage) {gc(verbose = FALSE)}
          
        }
        
        step[[2]] <- (min(step[[2]]) + stride[length(stride)]):min(max(step[[2]]) + stride[length(stride)], nrow(data[[1]]))
        
      }
      
      step[[1]] <- (min(step[[1]]) + stride[1]):min(max(step[[1]]) + stride[1], ncol(data[[1]]))
        
    }
    
    preds$ID <- NULL
    
    return(list(model = model,
                preds = preds,
                dimensions = dimensions,
                multi_class = multi_class,
                folds = folds,
                work_dir = model_path))
    
  }
  
}
