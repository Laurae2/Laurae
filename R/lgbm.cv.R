#' LightGBM Cross-Validated Model Training
#'
#' This function allows you to cross-validate a LightGBM model.
#' It is recommended to have your x_train and x_val sets as data.table, and to use the development data.table version.
#' To install data.table development version, please run in your R console: \code{install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")}.
#' The speed increase to create the train and test files can exceed 100x over write.table in certain cases.
#' 
#' The most important parameters are \code{lgbm_path} and \code{workingdir}: they setup where LightGBM is and where temporary files are going to be stored. \code{lgbm_path} is the full path to LightGBM executable, and includes the executable name and file extension (like \code{C:/Laurae/LightGBM/windows/x64/Release/LightGBM.exe}). \code{workingdir} is the working directory for the temporary files for LightGBM. It creates a lot of necessary files to make LightGBM work (defined by \code{output_model, output_result, train_conf, train_name, val_name, pred_conf}).
#' 
#' \code{train_conf}, \code{train_name}, and \code{val_name} defines respectively the configuration file name, the train file name, and the validation file name. They are created under this name when \code{files_exist} is set to \code{FALSE}.
#' 
#' \code{unicity} defines whether to create separate files (if \code{TRUE}) or to save space by writing over the same file (if \code{FALSE}). Predicting does not work with \code{FALSE}.
#' 
#' Once you filled these variables (and if they were appropriate), you should fill \code{y_train, x_train}. If you need model validation, fill also \code{y_val, x_val}. y is your label (a vector), while x is your data.table (preferred) or a data.frame or a matrix.
#' 
#' Then, you are up to choose what you want, including hyperparameters to verbosity control.
#' 
#' If for some reason you lose the ability to print in the console, run \code{sink()} in the console several times until you get an error.
#' 
#' @param y_train Type: vector. The training labels.
#' @param x_train Type: data.table (preferred), data.frame, or matrix. The training features.
#' @param folds Type: vector of integers. The fold assigned to each row.
#' @param application Type: character. The label application to learn. Must be either \code{'regression'}, \code{'binary'}, or \code{'lambdarank'}. Defaults to \code{'regression'}.
#' @param validation Type: boolean. Whether LightGBM performs validation during the training, by outputting metrics for the validation data. Defaults to \code{TRUE}. Multi-validation data is not supported yet.
#' @param num_iterations Type: integer. The number of boosting iterations LightGBM will perform. Defaults to \code{10}.
#' @param early_stopping_rounds Type: integer. The number of boosting iterations whose validation metric is lower than the best is required for LightGBM to automatically stop. Defaults to \code{NA}.
#' @param learning_rate Type: numeric. The shrinkage rate applied to each iteration. Lower values lowers overfitting speed, while higher values increases overfitting speed. Defaults to \code{0.1}.
#' @param num_leaves Type: integer. The number of leaves in one tree. Roughly, a recommended value is \code{n^2 - 1}, \code{n} being the theoretical depth if each tree were identical. Lower values lowers tree complexity, while higher values increases tree complexity. Defaults to \code{127}.
#' @param tree_learner Type: character. The type of learner use, between \code{'serial'} (single machine tree learner), \code{'feature'} (feature parallel tree learner), \code{'data'} (data parallel tree learner). Defaults to \code{'serial'}. Other learners are not supported yet. (?)
#' @param num_threads Type: integer. The number of threads to run for LightGBM. It is recommended to not set it higher than the amount of physical cores in your computer. Defaults to \code{2}. In virtualized environments, it can be better to set it to the maximum amount of threads allocated to the virtual machine (especially VirtualBox).
#' @param min_data_in_leaf Type: integer. Minimum number of data in one leaf. Higher values potentially decrease overfitting. Defaults to \code{100}.
#' @param min_sum_hessian_in_leaf Type: numeric. Minimum sum of hessians in one leaf to allow a split. Higher values potentially decrease overfitting. Defaults to \code{10.0}.
#' @param feature_fraction Type: numeric (0, 1). Column subsampling percentage. For instance, 0.5 means selecting 50\% of features randomly for each iteration. Lower values potentially decrease overfitting, while training faster. Defaults to \code{1.0}.
#' @param feature_fraction_seed Type: integer. Random starting seed for the column subsampling (\code{feature_fraction}). Defaults to \code{2}.
#' @param bagging_fraction Type: numeric (0, 1). Row subsampling percentage. For instance, 0.5 means selecting 50\% of rows randomly for each iteration. Lower values potentially decrease overfitting, while training faster. Defaults to \code{1.0}. Unused when \code{bagging_freq} is \code{0}.
#' @param bagging_freq Type: integer. The frequency of row subsampling (\code{bagging_fraction}). Lower values potentially decrease overfitting, while training faster. Defaults to \code{0}.
#' @param bagging_seed Type: integer. Random starting seed for the row subsampling (\code{bagging_fraction}). Defaults to \code{3}.
#' @param max_bin Type: integer. The maximum number of bins created per feature. Lower values potentially decrease overfitting. Defaults to \code{255}.
#' @param data_random_seed Type: integer. Random starting seed for the parallel learner. Defaults to \code{1}.
#' @param data_has_label Type: boolean. Whether the data has labels or not. Do not modify this. Defaults to \code{TRUE}.
#' @param output_model Type: character. The file name of output model. Defaults to \code{'lgbm_model.txt'}.
#' @param input_model Type: characer. The file name of input model. If defined, LightGBM will resume training from that file. Defaults to \code{NA}. Unused yet.
#' @param output_result Type: character. The file name of the prediction results for the model. Defaults to \code{'lgbm_predict.txt'}. Unused yet.
#' @param is_sigmoid Type: boolean. Whether to use a sigmoid transformation of raw predictions. Defaults to \code{TRUE}.
#' @param init_score Type: string. The file name of initial scores to start training LightGBM. Defaults to \code{''}. Automatic creation of the initial scores is not implemented yet.
#' @param is_pre_partition Type: boolean. Whether data is pre-partitioned for parallel learning. Defaults to \code{FALSE}. Unused.
#' @param is_sparse Type: boolean. Whether sparse optimization is enabled. Defaults to \code{TRUE}.
#' @param two_round Type: boolean. LightGBM maps data file to memory and load features from memory to maximize speed. If the data is too large to fit in memory, use TRUE. Defaults to \code{FALSE}.
#' @param save_binary Type: boolean. Whether data should be saved as binary files for faster load. Defaults to \code{FALSE}.,
#' @param sigmoid Type: numeric. "The sigmoid parameter". Defaults to \code{1.0}.
#' @param is_unbalance Type: boolean. For binary classification, setting this to TRUE might be useful when the training data is unbalanced. Defaults to \code{FALSE}.
#' @param max_position Type: integer. For lambdarank, optimize NDCG for that specific value. Defaults to \code{20}.
#' @param label_gain Type: vector of integers. For lambdarank, relevant gain for labels. Defaults to \code{c(0, 1, 3, 7, 15, 31, 63)}.
#' @param metric Type: character, or vector of characters. The metric to optimize. There are 6 available: \code{'l1'} (absolute loss), \code{'l2'} (squared loss), \code{'ndcg'} (NDCG), \code{'auc'} (AUC), \code{'binary_logloss'} (logarithmic loss), and \code{'binary_error'} (accuracy). Defaults to \code{'l2'}. Use a vector of characters to pass multiple metrics.
#' @param metric_freq Type: integer. The frequency to report the metric(s). Defaults to \code{1}.
#' @param is_training_metric Type: boolean. Whether to report the training metric in addition to the validation metric. Defaults to \code{FALSE}.
#' @param ndcg_at Type: vector of integers. Evaluate NDCG metric at these values. Defaults to \code{c(1, 2, 3, 4, 5)}.
#' @param num_machines Type: integer. When using parallel learning, the number of machines to use. Defaults to \code{1}.
#' @param local_listen_port Type: integer. The TCP listening port for the local machines. Allow this port in the firewall before training. \code{12400}.
#' @param time_out Type: integer. The socket time-out in minutes. Defaults to \code{120}.
#' @param machine_list_file Type: character. The file that contains the machine list for parallel learning. A line in that file much correspond to one IP and one port for one machine, separated by space instead of a colon (\code{:}). Defaults to \code{''}.
#' @param lgbm_path Type: character. Where is stored LightGBM? Include only the folder to it. Defaults to \code{'path/to/LightGBM.exe'}.
#' @param workingdir Type: character. The working directory used for LightGBM. Defaults to \code{getwd()}.
#' @param files_exist Type: boolean. Whether the files are already existing. It does not export the files anymore if the training and validation files were already exported previously. Defaults to \code{FALSE}.
#' @param train_conf Type: character. The name of the train_conf file for the model. Defaults to \code{'lgbm_train.conf'}
#' @param train_name Type: character. The name of the training data file for the model. Defaults to \code{'lgbm_train.csv'}
#' @param val_name Type: character. The name of the testing data file for the model. Defaults to \code{'lgbm_val.csv'}
#' @param unicity Type: boolean. Whether to overwrite each train/validation file. If not, adds a tag to each file. Defaults to \code{TRUE}.
#' @param predictions Type: boolean. Whether cross-validated predictions should be returned. Defaults to \code{TRUE}.
#' @param pred_conf Type: character. The name of the pred_conf file for the model. Defaults to \code{'lgbm_pred.conf'}
#' @param verbose Type: boolean. Whether to print a lot of debug messages or not. Using a defined \code{log_name} and \code{verbose = TRUE} is equivalent to tee (output log to stdout and to a file). 0 is FALSE and 1 is TRUE. 2 can be used if you wish to not separate logs per fold (i.e. all log in one file + print in console), and -1 for not printing in console (keep only log). Defaults to \code{TRUE}. Useless as \code{FALSE} when log_name is not set. Might not work when your lgbm_path has a space. When FALSE, the default printing is diverted to \code{"diverted_verbose.txt"}, but the model log is output to \code{log_name}.
#' @param log_name Type: character. The logging (sink) file to output (like 'log.txt'). Defaults to \code{NA}.
#' @param log_append Type: boolean. Whether logging should be appended to the log_name or not (not delete or delete old). Defaults to \code{TRUE}.
#' 
#' @return A list of LightGBM models whose structure is defined in lgbm.train documentation in Value.
#' 
#' @examples
#' \dontrun{
#' 3-fold cross-validated LightGBM with log on a file.
#' Runs on 2 threads.
#' 1000 iterations with shrinkage of 0.1, stop when 10 iterations are not increase performance.
#' 127 leaves for an "approximate equivalent" of depth = 7.
#' Uses the working dir / temp folder as working directory for the temporary files.
#' trained.cv <- lgbm.cv(y_train = targets,
#'                       x_train = data[1:1500, ],
#'                       folds = c(rep(1, 500), rep(2, 500), rep(3, 500)),
#'                       application = "regression",
#'                       validation = TRUE,
#'                       num_iterations = 10,
#'                       learning_rate = 0.1,
#'                       num_leaves = 127,
#'                       tree_learner = "serial",
#'                       num_threads = 2,
#'                       lgbm_path = "C:/LightGBM/windows/x64/Release/lightgbm.exe",
#'                       workingdir = file.path(getwd(), "temp"),
#'                       files_exist = FALSE,
#'                       unicity = FALSE,
#'                       train_conf = 'lgbm_train.conf',
#'                       train_name = 'lgbm_train.csv',
#'                       val_name = 'lgbm_val.csv',
#'                       verbose = FALSE,
#'                       log_name = "houseprice_log_cv.txt")
#' }
#' 
#' @export

lgbm.cv <- function(
  y_train,
  x_train,
  folds,
  application = 'regression',
  validation = TRUE,
  num_iterations = 10,
  early_stopping_rounds = NA,
  learning_rate = 0.1,
  num_leaves = 127,
  tree_learner = 'serial',
  num_threads = 2,
  min_data_in_leaf = 100,
  min_sum_hessian_in_leaf = 10.0,
  feature_fraction = 1.0,
  feature_fraction_seed = 2,
  bagging_fraction = 1.0,
  bagging_freq = 0,
  bagging_seed = 3,
  max_bin = 255,
  data_random_seed = 1,
  data_has_label = TRUE,
  output_model = 'lgbm_model.txt',
  input_model = NA,
  output_result = 'lgbm_predict.txt',
  is_sigmoid = TRUE,
  init_score = '',
  is_pre_partition = FALSE,
  is_sparse = TRUE,
  two_round = FALSE,
  save_binary = FALSE,
  sigmoid = 1.0,
  is_unbalance = FALSE,
  max_position = 20,
  label_gain = c(0, 1, 3, 7, 15, 31, 63),
  metric = 'l2',
  metric_freq = 1,
  is_training_metric = FALSE,
  ndcg_at = c(1, 2, 3, 4, 5),
  num_machines = 1,
  local_listen_port = 12400,
  time_out = 120,
  machine_list_file = '',
  lgbm_path = 'path/to/LightGBM.exe',
  workingdir = getwd(),
  files_exist = FALSE,
  train_conf = 'lgbm_train.conf',
  train_name = 'lgbm_train.csv',
  val_name = 'lgbm_val.csv',
  unicity = FALSE,
  predictions = TRUE,
  pred_conf = 'lgbm_pred.conf',
  verbose = TRUE,
  log_name = NA,
  log_append = FALSE
) {
  
  outputs <- list()
  outputs[["Models"]] <- list()
  folds_list <- unique(folds)
  gc(verbose = FALSE)
  
  if (predictions) {
    preds <- numeric(length(folds))
  }
  
  # Attempts to speed up
  if (is.data.table(x_train) == FALSE) {
    setDT(x_train)
  }
  
  for (i in 1:length(folds_list)) {
    
    if (verbose > 0) cat('  \n************  \n', paste('Fold no:',i), '  \n************  \n', sep = "")
    
    # Create folds
    x_tr <- DTsubsample(DT = x_train, kept = which(folds != i), low_mem = FALSE, collect = 100, silent = TRUE)
    #x_tr <- x_train[folds != i,]
    gc(verbose = FALSE)
    x_val <- DTsubsample(DT = x_train, kept = which(folds == i), low_mem = FALSE, collect = 100, silent = TRUE)
    #x_val <- x_train[folds == i,]
    gc(verbose = FALSE)
    
    # Train
    outputs[["Models"]][[as.character(i)]] <- lgbm.train(
      x_train = x_tr,
      y_train = y_train[folds != i],
      x_val = x_val,
      y_val = y_train[folds == i],
      application = application,
      validation = validation,
      num_iterations = num_iterations,
      early_stopping_rounds = early_stopping_rounds,
      learning_rate = learning_rate,
      num_leaves = num_leaves,
      tree_learner = tree_learner,
      num_threads = num_threads,
      min_data_in_leaf = min_data_in_leaf,
      min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
      feature_fraction = feature_fraction,
      feature_fraction_seed = feature_fraction_seed,
      bagging_fraction = bagging_fraction,
      bagging_freq = bagging_freq,
      bagging_seed = bagging_seed,
      max_bin = max_bin,
      data_random_seed = data_random_seed,
      data_has_label = data_has_label,
      output_model = ifelse(!unicity, stri_replace_last_fixed(output_model, ".", paste0("_", i, ".")), output_model),
      input_model = ifelse(is.na(!unicity), NA, ifelse(!unicity, stri_replace_last_fixed(input_model, ".", paste0("_", i, ".")), input_model)),
      output_result = ifelse(!unicity, stri_replace_last_fixed(output_result, ".", paste0("_", i, ".")), output_result),
      is_sigmoid = is_sigmoid,
      init_score = init_score,
      is_pre_partition = is_pre_partition,
      is_sparse = is_sparse,
      two_round = two_round,
      save_binary = save_binary,
      sigmoid = sigmoid,
      is_unbalance = is_unbalance,
      max_position = max_position,
      label_gain = label_gain,
      metric = metric,
      metric_freq = metric_freq,
      is_training_metric = is_training_metric,
      ndcg_at = ndcg_at,
      num_machines = num_machines,
      local_listen_port = local_listen_port,
      time_out = time_out,
      machine_list_file = machine_list_file,
      lgbm_path = lgbm_path,
      workingdir = workingdir,
      files_exist = files_exist,
      train_conf = ifelse(!unicity, stri_replace_last_fixed(train_conf, ".", paste0("_", i, ".")), train_conf),
      train_name = ifelse(!unicity, stri_replace_last_fixed(train_name, ".", paste0("_", i, ".")), train_name),
      val_name = ifelse(!unicity, stri_replace_last_fixed(val_name, ".", paste0("_", i, ".")), val_name),
      verbose = as.logical(verbose),
      log_name = ifelse(!((!is.na(log_name)) & (verbose %in% c(0, 1))), stri_replace_last_fixed(file.path(workingdir, log_name), ".", paste0("_", i, ".")), log_name),
      log_append = (log_append & (verbose %in% c(0, 1))),
      predictions = predictions,
      pred_conf = ifelse(!unicity, stri_replace_last_fixed(pred_conf, ".", paste0("_", i, ".")), pred_conf)
      )
    if (predictions) {
      preds[folds == i] <- outputs[["Models"]][[as.character(i)]][["Predictions"]]
    }
  }
  
  if (predictions) {
    outputs[["Predictions"]] <- preds
  }
  
  return(outputs)
}