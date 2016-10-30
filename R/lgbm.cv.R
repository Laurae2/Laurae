#' LightGBM Cross-Validated Model Training
#'
#' This function allows you to cross-validate a LightGBM model.
#' It is recommended to have your x_train and x_val sets as data.table, and to use the development data.table version.
#' To install data.table development version, please run in your R console: \code{install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")}.
#' The speed increase to create the train and test files can exceed 1,000x over write.table in certain cases.
#' To store evaluation metrics throughout the training, you MUST run this function with \code{verbose = FALSE}.
#' 
#' The most important parameters are \code{lgbm_path} and \code{workingdir}: they setup where LightGBM is and where temporary files are going to be stored. \code{lgbm_path} is the full path to LightGBM executable, and includes the executable name and file extension (like \code{C:/Laurae/LightGBM/windows/x64/Release/LightGBM.exe}). \code{workingdir} is the working directory for the temporary files for LightGBM. It creates a lot of necessary files to make LightGBM work (defined by \code{output_model, output_preds, train_conf, train_name, val_name, pred_conf}).
#' 
#' \code{train_conf}, \code{train_name}, and \code{val_name} defines respectively the configuration file name, the train file name, and the validation file name. They are created under this name when \code{files_exist} is set to \code{FALSE}.
#' 
#' \code{unicity} defines whether to create separate files (if \code{TRUE}) or to save space by writing over the same file (if \code{FALSE}). Predicting does not work with \code{FALSE}. Files are taking the names you provided (or the default ones) while adding a "_X" to the file name before the file extension if \code{unicity = FALSE}.
#' 
#' Once you filled these variables (and if they were appropriate), you should fill \code{y_train, x_train}. If you need model validation, fill also \code{y_val, x_val}. y is your label (a vector), while x is your data.table (preferred) or a data.frame or a matrix.
#' 
#' Then, you are up to choose what you want, including hyperparameters to verbosity control.
#' 
#' To get the metric tables, you MUST use \code{verbose = FALSE}. It cannot be fetched without. \code{sink()} does not work.
#' 
#' If for some reason you lose the ability to print in the console, run \code{sink()} in the console several times until you get an error.
#' 
#' @param y_train Type: vector. The training labels.
#' @param x_train Type: data.table (preferred), data.frame, or matrix. The training features. Not providing a data.frame or a matrix results in at least 3x memory usage.
#' @param bias_train Type: numeric or vector of numerics. The initial weights of the training data. If a numeric is provided, then the weights are identical for all the training samples. Otherwise, use the vector as weights. Defaults to \code{NA}.
#' @param x_test Type: data.table (preferred), data.frame, or matrix. The testing features, if necessary. Not providing a data.frame or a matrix results in at least 3x memory usage. Defaults to \code{NA}. Predictions are averaged. Must be unlabeled.
#' @param data_has_label Type: boolean. Whether the data has labels or not. Do not modify this. Defaults to \code{TRUE}.
#' @param NA_value Type: numeric or character. What value replaces NAs. Use \code{"na"} if you want to specify "missing". It is not recommended to use something else, even by soemthing like a numeric value out of bounds (like \code{-999} if all your values are greater than \code{-999}). You should change from the default \code{"na"} if they have a real numeric meaning. Defaults to \code{"na"}.
#' @param lgbm_path Type: character. Where is stored LightGBM? Include only the folder to it. Defaults to \code{'path/to/LightGBM.exe'}.
#' @param workingdir Type: character. The working directory used for LightGBM. Defaults to \code{getwd()}.
#' @param train_name Type: character. The name of the default training data file for the model. Defaults to \code{'lgbm_train.csv'}.
#' @param val_name Type: character. The name of the default validation data file for the model. Defaults to \code{'lgbm_val.csv'}.
#' @param test_name Type: character. The name of the testing data file for the model. Defaults to \code{'lgbm_test.csv'}.
#' @param init_score Type: string. The file name of initial (bias) training scores to start training LightGBM, which contains \code{bias_train} values. Defaults to \code{ifelse(is.na(bias_train), NA, paste(train_name, ".weight", sep = ""))}, which means \code{NA} if \code{bias_train} is left default, else appends \code{".weight"} extension to \code{train_name} name.
#' @param files_exist Type: boolean. Whether the training (and testing) files are already existing. It overwrites files if there are any existing. Defaults to \code{FALSE}.
#' @param save_binary Type: boolean. Whether data should be saved as binary files for faster load. The name takes automatically the name from the \code{train_name} and adds the extension \code{".bin"}. Defaults to \code{FALSE}.,
#' @param train_conf Type: character. The name of the training configuration file for the model. Defaults to \code{'lgbm_train.conf'}.
#' @param pred_conf Type: character. The name of the prediction configuration file for the model. Defaults to \code{'lgbm_pred.conf'}.
#' @param test_conf Type: character. The name of the testing prediction configuration file for the model. Defaults to \code{'lgbm_test.conf'}.
#' @param validation Type: boolean. Whether LightGBM performs validation during the training, by outputting metrics for the validation data. Defaults to \code{TRUE}. Multi-validation data is not supported yet.
#' @param unicity Type: boolean. Whether to overwrite each train/validation file. If not, adds a tag to each file. Defaults to \code{TRUE}.
#' @param folds Type: integer, vector of two integers, vector of integers, or list. If a integer is supplied, performs a \code{folds}-fold cross-validation. If a vector of two integers is supplied, performs a \code{folds[1]}-fold cross-validation repeated \code{folds[2]} times. If a vector of integers (larger than 2) was provided, each integer value should refer to the fold, of the same length of the training data. Otherwise (if a list was provided), each element of the list must refer to a fold and they will be treated sequentially. Defaults to \code{5}.
#' @param stratified Type: boolean. Whether the folds should be stratified (keep the same label proportions) or not. Defaults to \code{TRUE}.
#' @param fold_seed Type: integer or vector of integers. The seed for the random number generator. If a vector of integer is provided, its length should be at least longer than \code{n}. Otherwise (if an integer is supplied), it starts each fold with the provided seed, and adds 1 to the seed for every repeat. Defaults to \code{0}.
#' @param folds_weight Type: vector of numerics. The weights assigned to each fold. If no weight is supplied (\code{NA}), the weights are automatically set to \code{rep(1/length(folds))} for an average (does not mix well with folds with different sizes). When the folds are automatically created by supplying \code{fold} a vector of two integers, then the weights are automatically computed. Defaults to \code{NA}.
#' @param fold_cleaning Type: integer. When using cross-validation, data must be subsampled. This parameter controls how aggressive RAM usage should be against speed. The lower this value, the more aggressive the method to keep memory usage as low as possible. Defaults to \code{50}.
#' @param predictions Type: boolean. Whether cross-validated predictions should be returned. Defaults to \code{TRUE}.
#' @param predict_leaf_index Type: boolean. When \code{predictions} is \code{TRUE}, should LightGBM predict leaf indexes? Defaults to \code{FALSE}. It is nearly mandatory to keep it \code{FALSE} unless you know what you are doing, as then you should use \code{separate_folds} to nto have a mix of non sense predictions.
#' @param separate_val Type: boolean. Whether out of fold predictions should be returned separately as raw as possible (a list with the predictions, and another ilst with the averaged predictions). Defaults to \code{TRUE}.
#' @param separate_tests Type: boolean. Whether weighted testing predictions should be returned separately as raw as possible (a list with the predictions, and another ilst with the averaged predictions). Defaults to \code{TRUE}.
#' @param output_preds Type: character. The file name of the prediction results for the model. Defaults to \code{'lgbm_predict.txt'}. Original name is \code{output_result}.
#' @param test_preds Type: character. The file name of the prediction results for the model. Defaults to \code{'lgbm_predict_test.txt'}.
#' @param verbose Type: boolean/integer. Whether to print a lot of debug messages in the console or not. 0 is FALSE and 1 is TRUE. Defaults to \code{TRUE}. When set to \code{FALSE}, the model log is output to \code{log_name} which allows to get metric information from the \code{log_name} parameter!!!
#' @param log_name Type: character. The logging (sink) file to output (like 'log.txt'). Defaults to \code{'lgbm_log.txt'}.
#' @param full_quiet Type: boolean. Whether file writing is quiet or not. When set to \code{TRUE}, the default printing is diverted to \code{'diverted_verbose.txt'}. Combined with \code{verbose = FALSE}, the function is fully quiet. Defaults to \code{FALSE}.
#' @param full_console Type: boolean. Whether a dedicated console should be visible. Defaults to \code{FALSE}.
#' @param importance Type: boolean. Should LightGBM perform feature importance? Defaults to \code{FALSE}.
#' @param output_model Type: character. The file name of output model. Defaults to \code{'lgbm_model.txt'}.
#' @param input_model Type: character. The file name of input model. You MUST user a different \code{output_model} file name if you define \code{input_model}. Otherwise, you are overwriting your model (and if your model cannot learn by stopping immediately at the beginning, you would LOSE your model). If defined, LightGBM will resume training from that file. Defaults to \code{NA}. Unused yet.
#' @param num_threads Type: integer. The number of threads to run for LightGBM. It is recommended to not set it higher than the amount of physical cores in your computer. Defaults to \code{2}. In virtualized environments, it can be better to set it to the maximum amount of threads allocated to the virtual machine (especially VirtualBox).
#' @param histogram_pool_size Type: integer. The maximum cache size (in MB) allocated for LightGBM histogram sketching. Values below \code{0} (like \code{-1}) means no limit. Defaults to \code{-1}.
#' @param is_sparse Type: boolean. Whether sparse optimization is enabled. Defaults to \code{TRUE}.
#' @param two_round Type: boolean. LightGBM maps data file to memory and load features from memory to maximize speed. If the data is too large to fit in memory, use TRUE. Defaults to \code{FALSE}.
#' @param application Type: character. The label application to learn. Must be either \code{'regression'}, \code{'binary'}, or \code{'lambdarank'}. Defaults to \code{'regression'}.
#' @param learning_rate Type: numeric. The shrinkage rate applied to each iteration. Lower values lowers overfitting speed, while higher values increases overfitting speed. Defaults to \code{0.1}.
#' @param num_iterations Type: integer. The number of boosting iterations LightGBM will perform. Defaults to \code{10}.
#' @param early_stopping_rounds Type: integer. The number of boosting iterations whose validation metric is lower than the best is required for LightGBM to automatically stop. Defaults to \code{NA}.
#' @param num_leaves Type: integer. The number of leaves in one tree. Roughly, a recommended value is \code{n^2 - 1}, \code{n} being the theoretical depth if each tree were identical. Lower values lowers tree complexity, while higher values increases tree complexity. Defaults to \code{127}.
#' @param min_data_in_leaf Type: integer. Minimum number of data in one leaf. Higher values potentially decrease overfitting. Defaults to \code{100}.
#' @param min_sum_hessian_in_leaf Type: numeric. Minimum sum of hessians in one leaf to allow a split. Higher values potentially decrease overfitting. Defaults to \code{10.0}.
#' @param max_bin Type: integer. The maximum number of bins created per feature. Lower values potentially decrease overfitting. Defaults to \code{255}.
#' @param feature_fraction Type: numeric (0, 1). Column subsampling percentage. For instance, 0.5 means selecting 50\% of features randomly for each iteration. Lower values potentially decrease overfitting, while training faster. Defaults to \code{1.0}.
#' @param feature_fraction_seed Type: integer. Random starting seed for the column subsampling (\code{feature_fraction}). Defaults to \code{2}.
#' @param bagging_fraction Type: numeric (0, 1). Row subsampling percentage. For instance, 0.5 means selecting 50\% of rows randomly for each iteration. Lower values potentially decrease overfitting, while training faster. Defaults to \code{1.0}. Unused when \code{bagging_freq} is \code{0}.
#' @param bagging_freq Type: integer. The frequency of row subsampling (\code{bagging_fraction}). Lower values potentially decrease overfitting, while training faster. Defaults to \code{0}.
#' @param bagging_seed Type: integer. Random starting seed for the row subsampling (\code{bagging_fraction}). Defaults to \code{3}.
#' @param is_sigmoid Type: boolean. Whether to use a sigmoid transformation of raw predictions. Defaults to \code{TRUE}.
#' @param sigmoid Type: numeric. "The sigmoid parameter". Defaults to \code{1.0}.
#' @param is_unbalance Type: boolean. For binary classification, setting this to TRUE might be useful when the training data is unbalanced. Defaults to \code{FALSE}.
#' @param max_position Type: integer. For lambdarank, optimize NDCG for that specific value. Defaults to \code{20}.
#' @param label_gain Type: vector of integers. For lambdarank, relevant gain for labels. Defaults to \code{c(0, 1, 3, 7, 15, 31, 63)}.
#' @param metric Type: character, or vector of characters. The metric to optimize. There are 6 available: \code{'l1'} (absolute loss), \code{'l2'} (squared loss), \code{'ndcg'} (NDCG), \code{'auc'} (AUC), \code{'binary_logloss'} (logarithmic loss), and \code{'binary_error'} (accuracy). Defaults to \code{'l2'}. Use a vector of characters to pass multiple metrics.
#' @param metric_freq Type: integer. The frequency to report the metric(s). Defaults to \code{1}.
#' @param is_training_metric Type: boolean. Whether to report the training metric in addition to the validation metric. Defaults to \code{FALSE}.
#' @param ndcg_at Type: vector of integers. Evaluate NDCG metric at these values. Defaults to \code{c(1, 2, 3, 4, 5)}.
#' @param tree_learner Type: character. The type of learner use, between \code{'serial'} (single machine tree learner), \code{'feature'} (feature parallel tree learner), \code{'data'} (data parallel tree learner). Defaults to \code{'serial'}.
#' @param is_pre_partition Type: boolean. Whether data is pre-partitioned for parallel learning. Defaults to \code{FALSE}.
#' @param data_random_seed Type: integer. Random starting seed for the parallel learner. Defaults to \code{1}.
#' @param num_machines Type: integer. When using parallel learning, the number of machines to use. Defaults to \code{1}.
#' @param local_listen_port Type: integer. The TCP listening port for the local machines. Allow this port in the firewall before training. \code{12400}.
#' @param time_out Type: integer. The socket time-out in minutes. Defaults to \code{120}.
#' @param machine_list_file Type: character. The file that contains the machine list for parallel learning. A line in that file much correspond to one IP and one port for one machine, separated by space instead of a colon (\code{:}). Defaults to \code{''}.
#' 
#' @return A list of LightGBM models whose structure is defined in lgbm.train documentation in Value. Returns a list of character variables if LightGBM is not found under lgbm_path. In addition, weighted out of fold predictions \code{Validation} are provided if \code{predictions} is set to \code{TRUE}, and weighted averaged testing predictions \code{Testing} are provided if \code{predictions} is set to \code{TRUE} with a testing set, and weights \code{Weights} if \code{predictions} is set to \code{TRUE}. Also, aggregated feature importance is provided if \code{importance} is set to \code{TRUE}.
#' 
#' @examples
#' \dontrun{
#' 3-fold cross-validated LightGBM with log on a file, with predictions and feature importance.
#' Runs on 2 threads.
#' 1000 iterations with shrinkage of 0.1, stop when 10 iterations are not increase performance.
#' 127 leaves for an "approximate equivalent" of depth = 7.
#' Uses the working dir / temp folder as working directory for the temporary files.
#' Returns in addition the out of fold predictions, and the feature importance.
#' trained.cv <- lgbm.cv(y_train = targets,
#'                       x_train = data[1:1500, ],
#'                       lgbm_path = "C:/LightGBM/windows/x64/Release/lightgbm.exe",
#'                       workingdir = file.path(getwd(), "temp"),
#'                       files_exist = FALSE,
#'                       train_conf = 'lgbm_train.conf',
#'                       train_name = 'lgbm_train.csv',
#'                       val_name = 'lgbm_val.csv',
#'                       validation = TRUE,
#'                       unicity = FALSE,
#'                       folds = 3,
#'                       verbose = FALSE,
#'                       log_name = "houseprice_log_cv.txt",
#'                       predictions = TRUE,
#'                       importance = TRUE,
#'                       num_threads = 2,
#'                       application = "regression",
#'                       num_iterations = 10,
#'                       learning_rate = 0.1,
#'                       num_leaves = 127)
#' }
#' 
#' @export

lgbm.cv <- function(
  
  # Data-related
  y_train,
  x_train,
  bias_train = NA,
  x_test = NA,
  data_has_label = TRUE,
  NA_value = "nan",
  
  # LightGBM I/O-related
  lgbm_path = 'path/to/LightGBM.exe',
  workingdir = getwd(),
  
  # Data files to create/user
  train_name = 'lgbm_train.csv',
  val_name = 'lgbm_val.csv',
  test_name = 'lgbm_test.csv',
  init_score = ifelse(is.na(bias_train), NA, paste(train_name, ".weight", sep = "")),
  files_exist = FALSE,
  save_binary = FALSE,
  
  # Configuration files to create/user
  train_conf = 'lgbm_train.conf',
  pred_conf = 'lgbm_pred.conf',
  test_conf = 'lgbm_test.conf',
  
  # Validation method
  validation = TRUE,
  unicity = FALSE,
  folds = 5,
  folds_weight = NA,
  stratified = TRUE,
  fold_seed = 0,
  fold_cleaning = 50,
  
  # Prediction-related
  predictions = TRUE,
  predict_leaf_index = FALSE,
  separate_val = TRUE,
  separate_tests = TRUE,
  output_preds = 'lgbm_predict.txt',
  test_preds = 'lgbm_predict_test.txt',
  
  # Analysis-related
  verbose = TRUE,
  log_name = 'lgbm_log.txt',
  full_quiet = FALSE,
  full_console = FALSE,
  importance = FALSE,
  
  # Model storage
  output_model = 'lgbm_model.txt',
  input_model = NA,
  
  # Speed and RAM parameters
  num_threads = 2,
  histogram_pool_size = -1,
  is_sparse = TRUE,
  two_round = FALSE,
  
  # Model basic hyperparameters
  application = 'regression',
  learning_rate = 0.1,
  num_iterations = 10,
  early_stopping_rounds = NA,
  num_leaves = 127,
  min_data_in_leaf = 100,
  min_sum_hessian_in_leaf = 10.0,
  
  # Model sampling hyperparameters
  max_bin = 255,
  feature_fraction = 1.0,
  feature_fraction_seed = 2,
  bagging_fraction = 1.0,
  bagging_freq = 0,
  bagging_seed = 3,
  
  # Objective specific
  is_sigmoid = TRUE,
  sigmoid = 1.0,
  is_unbalance = FALSE,
  max_position = 20,
  label_gain = c(0, 1, 3, 7, 15, 31, 63),
  
  # Metrics
  metric = 'l2',
  metric_freq = 1,
  is_training_metric = FALSE,
  ndcg_at = c(1, 2, 3, 4, 5),
  
  
  # Machine-distributed learning parameters
  tree_learner = 'serial',
  is_pre_partition = FALSE,
  data_random_seed = 1,
  num_machines = 1,
  local_listen_port = 12400,
  time_out = 120,
  machine_list_file = ''
  
) {
  
  outputs <- list()
  outputs[["Models"]] <- list()
  
  # Attempts to unscramble "folds"
  if (!is.list(folds)) {
    # It's not the list case
    
    if (length(folds) == 1) {
      # It's the case of 1 integer value passed
      folds_list <- kfold(y = y_train, k = folds, stratified = stratified, seed = fold_seed)
      if (is.na(folds_weight[1])) {folds_weight <- rep(1/folds, folds)}
      
    } else {
      # It's not the case of 1 integer value passed
      
      if (length(folds) == 2) {
        # It's the case of 2 integers value passed
        folds_list <- nkfold(y = y_train, n = folds[2], k = folds[1], stratified = stratified, seed = fold_seed, weight = TRUE)
        if (is.na(folds_weight[1])) {folds_weight <- folds_list$Weights}
        folds_list <- folds_list$Folds
        
      } else {
        # It's the case of a vector of integers passed, so check length
        if (!(length(folds) == length(y_train))) {
          cat("Folds are not matching the training data. (Folds=", length(folds), " vs Train=", length(y_train), ")  \n", sep = "")
          return("Bad fold length!")
        } else {
          # Parse folds appropriately
          folds_list <- list()
          folds_unique <- unique(folds)
          for (i in 1:length(folds_unique)) {
            folds_list[[i]] <- which(folds == folds_unique[i])
          }
          if (is.na(folds_weight[1])) {folds_weight <- rep(1/length(folds_unique), length(folds_unique))}
          
        }
        
      }
      
    }
  } else {
    
    folds_list <- folds
    if (is.na(folds_weight[1])) {
      folds_weight <- rep(1/length(folds_list), length(folds_list))
    }
    
  }
  
  gc(verbose = FALSE)
  
  if (predictions) {
    preds_occ <- numeric(length(y_train))
    if (separate_val) {
      preds <- list()
      preds[[1]] <- numeric(length(y_train))
      preds[[2]] <- list()
    } else {
      preds <- numeric(length(y_train))
    }
    if (length(x_test) > 1) {
      tests_occ <- sum(folds_weight)
      if (separate_tests) {
        tests <- list()
        tests[[1]] <- numeric(nrow(x_test))
        tests[[2]] <- list()
      } else {
        tests <- numeric(nrow(x_test))
      }
    }
  }
  
  # Attempts to speed up - Disabled for now.
  # if (is.data.table(x_train) == FALSE) {
  #   setDT(x_train)
  # }
  
  # User does not want to provide x_train as data.table...
  # if (!is.data.table(x_train)) {
  #   x_train <- as.data.table(x_train)
  #   gc(verbose = FALSE)
  # }
  
  if (predictions) {
    
    for (i in 1:length(folds_list)) {
      preds_occ[folds_list[[i]]] <- preds_occ[folds_list[[i]]] + folds_weight[i]
    }
    
  }
  
  
  for (i in 1:length(folds_list)) {
    
    fold_shortcut <- sprintf(paste("%0", floor(log10(length(folds_list)) + 1), "d", sep = ""), i)
    if (!full_quiet) {
      cat(paste0('  \n  \n***************  \n', paste('Fold no: ', fold_shortcut), ' / ', length(folds_list), '  \n***************  \n'))
    }
    
    # Create folds
    if (!files_exist) {
      x_tr <- DTsubsample(DT = x_train, kept = (1:nrow(x_train))[-folds_list[[i]]], low_mem = FALSE, collect = fold_cleaning, silent = TRUE)
      gc(verbose = FALSE)
      x_val <- DTsubsample(DT = x_train, kept = folds_list[[i]], low_mem = FALSE, collect = fold_cleaning, silent = TRUE)
      gc(verbose = FALSE)
    } else {
      x_tr <- x_train[1, ]
      x_val = x_val[1, ]
    }
    
    # Train
    outputs[["Models"]][[as.character(i)]] <- lgbm.train(
      # Data-related
      x_train = x_tr,
      y_train = y_train[-folds_list[[i]]],
      bias_train = bias_train,
      x_val = x_val,
      y_val = y_train[folds_list[[i]]],
      data_has_label = data_has_label,
      NA_value = NA_value,
      
      # LightGBM-related
      lgbm_path = lgbm_path,
      workingdir = workingdir,
      
      # Data files to create/use
      train_name = ifelse(!unicity, stri_replace_last_fixed(train_name, ".", paste0("_", fold_shortcut, ".")), train_name),
      val_name = ifelse(!unicity, stri_replace_last_fixed(val_name, ".", paste0("_", fold_shortcut, ".")), val_name),
      init_score = init_score,
      files_exist = files_exist,
      save_binary = save_binary,
      
      # Configuration files to create/user
      train_conf = ifelse(!unicity, stri_replace_last_fixed(train_conf, ".", paste0("_", fold_shortcut, ".")), train_conf),
      pred_conf = ifelse(!unicity, stri_replace_last_fixed(pred_conf, ".", paste0("_", fold_shortcut, ".")), pred_conf),
      
      # Prediction-related
      validation = validation,
      predictions = predictions,
      predict_leaf_index = predict_leaf_index,
      output_preds = ifelse(!unicity, stri_replace_last_fixed(output_preds, ".", paste0("_", fold_shortcut, ".")), output_preds),
      
      # Analysis-related
      verbose = verbose,
      log_name = ifelse(!unicity, stri_replace_last_fixed(log_name, ".", paste0("_", fold_shortcut, ".")), log_name),
      full_quiet = full_quiet,
      full_console = full_console,
      importance = importance,
      
      # Model storage
      output_model = ifelse(!unicity, stri_replace_last_fixed(output_model, ".", paste0("_", fold_shortcut, ".")), output_model),
      input_model = ifelse(is.na(!input_model), NA, ifelse(!unicity, stri_replace_last_fixed(input_model, ".", paste0("_", fold_shortcut, ".")), input_model)),
      
      # Speed and RAM parameters
      num_threads = num_threads,
      histogram_pool_size = histogram_pool_size,
      is_sparse = is_sparse,
      two_round = two_round,
      
      # Model basic hyperparameters
      application = application,
      learning_rate = learning_rate,
      num_iterations = num_iterations,
      early_stopping_rounds = early_stopping_rounds,
      num_leaves = num_leaves,
      min_data_in_leaf = min_data_in_leaf,
      min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
      
      # Model sampling hyperparameters
      max_bin = max_bin,
      feature_fraction = feature_fraction,
      feature_fraction_seed = feature_fraction_seed,
      bagging_fraction = bagging_fraction,
      bagging_freq = bagging_freq,
      bagging_seed = bagging_seed,
      
      # Objective specific
      is_sigmoid = is_sigmoid,
      sigmoid = sigmoid,
      is_unbalance = is_unbalance,
      max_position = max_position,
      label_gain = label_gain,
      
      # Metrics
      metric = metric,
      metric_freq = metric_freq,
      is_training_metric = is_training_metric,
      ndcg_at = ndcg_at,
      
      # Machine-distributed learning parameters
      tree_learner = tree_learner,
      is_pre_partition = is_pre_partition,
      data_random_seed = data_random_seed,
      num_machines = num_machines,
      local_listen_port = local_listen_port,
      time_out = time_out,
      machine_list_file = machine_list_file,
    )
    
    # Catch predictions
    if (predictions) {
      
      if (separate_val) {
        preds[[2]][[i]] <- outputs[["Models"]][[i]][["Validation"]]
        preds[[1]][folds_list[[i]]] <- preds[[1]][folds_list[[i]]] + (outputs[["Models"]][[i]][["Validation"]] * folds_weight[i] / preds_occ[folds_list[[i]]])
      } else {
        preds[folds_list[[i]]] <- preds[folds_list[[i]]] + (outputs[["Models"]][[i]][["Validation"]] * folds_weight[i] / preds_occ[folds_list[[i]]])
      }
      
      if (length(x_test) > 1) {
        
        tests_preds <- lgbm.predict(
          model = '',
          y_pred = NA,
          x_pred = x_test,
          data_has_label = FALSE,
          lgbm_path = lgbm_path,
          workingdir = workingdir,
          input_model = ifelse(!unicity, stri_replace_last_fixed(output_model, ".", paste0("_", fold_shortcut, ".")), output_model),
          pred_conf = ifelse(!unicity, stri_replace_last_fixed(test_conf, ".", paste0("_", fold_shortcut, ".")), test_conf),
          predict_leaf_index = predict_leaf_index,
          verbose = verbose,
          data_name = test_name,
          files_exist = (!(i == 1) | files_exist),
          output_preds = ifelse(!unicity, stri_replace_last_fixed(test_preds, ".", paste0("_", fold_shortcut, ".")), test_preds),
          data.table = exists("data.table"))
        
        if (separate_tests) {
          tests[[2]][[i]] <- tests_preds
          tests[[1]] <- tests[[1]] + (tests_preds * folds_weight[i] / sum(folds_weight))
        } else {
          tests <- tests + (tests_preds * folds_weight[i] / sum(folds_weight))
        }
        
      }
      
    }
    
    # Catch importance
    if (importance) {
      if (i == 1) {
        important <- outputs[["Models"]][[i]][["FeatureImp"]][, c("Feature", "Gain", "Freq"), with = FALSE]
      } else {
        important <- rbind(important, outputs[["Models"]][[i]][["FeatureImp"]][, c("Feature", "Gain", "Freq"), with = FALSE])
      }
    }
  }
  
  if (predictions) {
    outputs[["Validation"]] <- preds
    if (length(x_test) > 1) {
      outputs[["Testing"]] <- tests
    }
    outputs[["Weights"]] <- folds_weight
  }
  
  if (importance) {
    freq_out <- important[, list(Freq = length(Gain), Gain = sum(Gain), Gain_Std = sd(Gain)), by = Feature]
    freq_out$Gain_Std[is.na(freq_out$Gain_Std)] <- 0
    freq_out[, Freq_Rel_Ratio := Freq/max(Freq)]
    freq_out[, Freq_Abs_Ratio := Freq/sum(Freq)]
    freq_out[, Gain_Rel_Ratio := Gain/max(Gain)]
    freq_out[, Gain_Abs_Ratio := Gain/sum(Gain)]
    freq_out[, Gain_Std_Rel_Ratio := Gain_Std/max(Gain_Std)]
    freq_out[, Gain_Std_Abs_Ratio := Gain_Std/sum(Gain_Std)]
    setcolorder(freq_out, c("Feature", "Gain", "Gain_Rel_Ratio", "Gain_Abs_Ratio", "Gain_Std", "Gain_Std_Rel_Ratio", "Gain_Std_Abs_Ratio", "Freq", "Freq_Rel_Ratio", "Freq_Abs_Ratio"))
    freq_out <- freq_out[order(Freq, Gain, decreasing = TRUE), ]
    outputs[["FeatureImp"]] <- freq_out
  }
  
  return(outputs)
}