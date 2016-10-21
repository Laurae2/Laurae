#' LightGBM Model Training
#'
#' This function allows you to train a LightGBM model.
#' It is recommended to have your x_train and x_val sets as data.table, and to use the development data.table version.
#' To install data.table development version, please run in your R console: \code{install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")}.
#' The speed increase to create the train and test files can exceed 100x over write.table in certain cases.
#' 
#' The most important parameters are \code{lgbm_path} and \code{workingdir}: they setup where LightGBM is and where temporary files are going to be stored. \code{lgbm_path} is the full path to LightGBM executable, and includes the executable name and file extension (like \code{C:/Laurae/LightGBM/windows/x64/Release/LightGBM.exe}). \code{workingdir} is the working directory for the temporary files for LightGBM. It creates a lot of necessary files to make LightGBM work (defined by \code{output_model, output_result, train_conf, train_name, val_name, pred_conf}).
#' 
#' \code{train_conf}, \code{train_name}, and \code{val_name} defines respectively the configuration file name, the train file name, and the validation file name. They are created under this name when \code{files_exist} is set to \code{FALSE}.
#' 
#' Once you filled these variables (and if they were appropriate), you should fill \code{y_train, x_train}. If you need model validation, fill also \code{y_val, x_val}. y is your label (a vector), while x is your data.table (preferred) or a data.frame or a matrix.
#' 
#' Then, you are up to choose what you want, including hyperparameters to verbosity control.
#' 
#' If for some reason you lose the ability to print in the console, run \code{sink()} in the console several times until you get an error.
#' 
#' @param y_train Type: vector. The training labels.
#' @param x_train Type: data.table (preferred), data.frame, or matrix. The training features.
#' @param y_val Type: vector. The validation labels. Defaults to \code{NULL}. Unused when \code{validation} is \code{TRUE}.
#' @param x_val Type: data.table (preferred), data.frame, or matrix. The validation features. Defaults to \code{NULL}. Unused when \code{validation} is \code{TRUE}.
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
#' @param output_result Type: character. The file name of the prediction results for the model. Defaults to \code{'lgbm_predict_result.txt'}. Unused yet.
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
#' @param verbose Type: boolean/integer. Whether to print a lot of debug messages or not. Using a defined \code{log_name} and \code{verbose = TRUE} is equivalent to tee (output log to stdout and to a file). 0 is FALSE and 1 is TRUE. Defaults to \code{TRUE}. Useless as \code{FALSE} when log_name is not set. Might not work when your lgbm_path has a space. When FALSE, the default printing is diverted to \code{"diverted_verbose.txt"}, but the model log is output to \code{log_name}.
#' @param log_name Type: character. The logging (sink) file to output (like 'log.txt'). Defaults to \code{NA}.
#' @param log_append Type: boolean. Whether logging should be appended to the log_name or not (not delete or delete old). Defaults to \code{TRUE}.
#' @param predictions Type: boolean. Should LightGBM compute predictions after training the model? Defaults to \code{FALSE}.
#' @param pred_conf Type: character. The name of the pred_conf file for the model. Defaults to \code{'lgbm_pred.conf'}.
#' 
#' @return A list with the stored trained model (\code{Model}), the path (\code{Path}) of the trained model, the name (\code{Name}) of the trained model file, the LightGBM path (\code{lgbm}) which trained the model, the training file name (\code{Train}), the testing file name even if there were none provided (\code{Test}), and the predictions (\code{Predictions}) if \code{predictions} is set to \code{TRUE}. Returns a character variable if LightGBM is not found under lgbm_path.
#' 
#' @examples
#' # Train a regression model on 1000 samples and validate on 1000 others.
#' # Runs on 2 threads.
#' # 1000 iterations with shrinkage of 0.1, stop when 10 iterations are not increase performance.
#' # 127 leaves for an "approximate equivalent" of depth = 7.
#' # Uses the working dir / temp folder as working directory for the temporary files.
#' # trained <- lgbm.train(y_train = targets[1:1000],
#' #                       x_train = data[1:1000, ],
#' #                       y_val = targets[1001:2000],
#' #                       x_val = data[1001:2000, ],
#' #                       application = "regression",
#' #                       validation = TRUE,
#' #                       num_iterations = 1000,
#' #                       early_stopping_rounds = 10,
#' #                       learning_rate = 0.1,
#' #                       num_leaves = 127,
#' #                       tree_learner = "serial",
#' #                       num_threads = 2,
#' #                       lgbm_path = "C:/xgboost/LightGBM/windows/x64/Release/lightgbm.exe",
#' #                       workingdir = file.path(getwd(), "temp"),
#' #                       files_exist = FALSE)
#' 
#' @export

lgbm.train <- function(
  y_train,
  x_train,
  y_val = NULL,
  x_val = NULL,
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
  output_result = 'lgbm_predict_result.txt',
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
  verbose = TRUE,
  log_name = NA,
  log_append = FALSE,
  predictions = FALSE,
  pred_conf = 'lgbm_pred.conf'
) {
  
  # Check file existance
  if(!file.exists(file.path(lgbm_path))){
    return(paste0('Could not find lightgbm.exe under ', file.path(lgbm_path), "."))
  }
  
  gc(verbose = FALSE)
  
  if (!is.na(log_name)) {
    sink(file = file.path(workingdir, "diverted_verbose.txt"), append = log_append, split = as.logical(verbose))
  }
  
  # Attempts to speed up
  if (is.data.table(x_train) == FALSE) {
    setDT(x_train)
    setDT(x_val)
  }
  
  # Setup working directory for LightGBM
  cat('Using LightGBM path: ', lgbm_path, "\n", sep = "")
  
  # Create working directory for LightGBM
  dir.create(file.path(workingdir), showWarnings = FALSE)
  cat('Working directory of LightGBM: ', file.path(workingdir), "\n", sep = "")
  
  # Setup the train configuration file
  #file.copy(paste0(lgbm_path), file.path(workingdir))
  fileConn <- file(file.path(workingdir, train_conf), "w")
  write(paste0('task=train'), fileConn, append = TRUE)
  write(paste0('application=',application), fileConn, append = TRUE)
  write(paste0('data="',file.path(workingdir, train_name), '"'), fileConn, append = TRUE)
  if (validation) write(paste0('valid="',file.path(workingdir, val_name), '"'), fileConn, append = TRUE)
  write(paste0('num_iterations=', num_iterations), fileConn, append = TRUE)
  if (!is.na(early_stopping_rounds)) write(paste0('early_stopping_rounds=', early_stopping_rounds), fileConn, append = TRUE)
  write(paste0('learning_rate=', learning_rate), fileConn, append = TRUE)
  write(paste0('num_leaves=', num_leaves), fileConn, append = TRUE)
  write(paste0('tree_learner=', tree_learner), fileConn, append = TRUE)
  write(paste0('num_threads=', num_threads), fileConn, append = TRUE)
  write(paste0('min_data_in_leaf=', min_data_in_leaf), fileConn, append = TRUE)
  write(paste0('min_sum_hessian_in_leaf=', min_sum_hessian_in_leaf), fileConn, append = TRUE)
  write(paste0('feature_fraction=', feature_fraction), fileConn, append = TRUE)
  write(paste0('feature_fraction_seed=', feature_fraction_seed), fileConn, append = TRUE)
  write(paste0('bagging_fraction=', bagging_fraction), fileConn, append = TRUE)
  write(paste0('bagging_freq=', bagging_freq), fileConn, append = TRUE)
  write(paste0('bagging_seed=', bagging_seed), fileConn, append = TRUE)
  write(paste0('max_bin=', max_bin), fileConn, append = TRUE)
  write(paste0('data_random_seed=', data_random_seed), fileConn, append = TRUE)
  write(paste0('data_has_label=', tolower(as.character(data_has_label))), fileConn, append = TRUE)
  if (output_model != '') write(paste0('output_model="', file.path(workingdir, output_model), '"'), fileConn, append = TRUE)
  write(paste0('is_sigmoid=', tolower(as.character(is_sigmoid))), fileConn, append = TRUE)
  if (init_score != '') write(paste0('init_score="',file.path(workingdir, init_score),'"'), fileConn, append = TRUE)
  write(paste0('is_pre_partition=', tolower(as.character(is_pre_partition))), fileConn, append = TRUE)
  write(paste0('is_sparse=', tolower(as.character(is_sparse))), fileConn, append = TRUE)
  write(paste0('two_round=', tolower(as.character(two_round))), fileConn, append = TRUE)
  write(paste0('save_binary=', tolower(as.character(save_binary))), fileConn, append = TRUE)
  write(paste0('sigmoid=', sigmoid), fileConn, append = TRUE)
  write(paste0('is_unbalance=', tolower(as.character(is_unbalance))), fileConn, append = TRUE)
  write(paste0('max_position=', max_position), fileConn, append = TRUE)
  write(paste0('label_gain=', paste(label_gain, collapse = ",")), fileConn, append = TRUE)
  write(paste0('metric=', paste(metric, collapse = ",")), fileConn, append = TRUE)
  write(paste0('metric_freq=', metric_freq), fileConn, append = TRUE)
  write(paste0('is_training_metric=', tolower(as.character(is_training_metric))), fileConn, append = TRUE)
  write(paste0('ndcg_at=', paste(ndcg_at, collapse = ",")), fileConn, append = TRUE)
  write(paste0('num_machines=', num_machines), fileConn, append = TRUE)
  write(paste0('local_listen_port=', local_listen_port), fileConn, append = TRUE)
  write(paste0('time_out=', time_out), fileConn, append = TRUE)
  if (machine_list_file != '') write(paste0('machine_list_file="', file.path(workingdir, machine_list_file), '"'), fileConn, append = TRUE)
  close(fileConn)
  cat('Training configuration file saved to: ', file.path(workingdir, train_conf), "\n", sep = "")
  
  # Export data
  if (!files_exist) {
    if (exists("fwrite") & is.data.table(x_train)) {
      # Uses the super fast CSV writer
      cat('Saving train data (data.table) file to: ', file.path(workingdir, train_name), sep = "")
      my_data <- x_train
      my_data$datatable_target <- y_train
      setcolorder(my_data, c("datatable_target", colnames(x_train)))
      fwrite(my_data, file.path = file.path(workingdir, train_name), col.names = FALSE, sep = ",", na = "nan")
    } else {
      # Fallback if no fwrite
      cat('Saving train data file to: ', file.path(workingdir, train_name), "\n", sep = "")
      write.table(cbind(y_train, x_train), file.path(workingdir, train_name), row.names = FALSE, col.names = FALSE, sep = ',', na = "nan")
      gc(verbose = FALSE) # In case of memory explosion
    }
    if (validation) {
      if (exists("fwrite") & is.data.table(x_train)) {
        cat('Saving validation data (data.table) file to: ', file.path(workingdir, val_name), "\n", sep = "")
        my_data <- x_val
        my_data$datatable_target <- y_val
        setcolorder(my_data, c("datatable_target", colnames(x_val)))
        fwrite(my_data, file.path = file.path(workingdir, val_name), col.names = FALSE, sep = ",", na = "nan")
      } else {
        # Fallback if no fwrite
        cat('Saving validation data file to: ', file.path(workingdir, val_name), "\n", sep = "")
        write.table(cbind(y_val, x_val), file.path(workingdir, val_name), row.names = FALSE, col.names = FALSE, sep = ',', na = "nan")
        gc(verbose = FALSE) # In case of memory explosion
      }
    }
  }
  
  gc(verbose = FALSE)
  if (verbose) {
    system(paste0('"', file.path(lgbm_path), '" config="', file.path(workingdir, train_conf), '"'), intern = !verbose)
  } else {
    invisible(system2(file.path(lgbm_path), args = paste0('config="', file.path(workingdir, train_conf), '"'), stdout = file.path(workingdir, log_name)))
  }
  cat('Model completed, results saved in ', file.path(workingdir), "\n", sep = "")
  
  output <- list()
  output[["Model"]] <- readLines(file.path(workingdir, output_model))
  output[["Path"]] <- file.path(workingdir)
  output[["Name"]] <- output_model
  output[["lgbm"]] <- file.path(lgbm_path)
  output[["Train"]] <- train_name
  output[["Test"]] <- val_name
  
  if (!is.na(log_name)) {
    sink()
  }
  
  gc(verbose = FALSE)
  if (predictions) {
    output[["Predictions"]] <- lgbm.predict(
      model = '',
      x_pred = NA,
      y_pred = NA,
      data_has_label = TRUE,
      val_name = val_name,
      input_model = output_model,
      output_result = output_result,
      lgbm_path = lgbm_path,
      workingdir = file.path(workingdir),
      files_exist = TRUE,
      pred_conf = pred_conf,
      data.table = exists("data.table"),
      verbose = verbose)
  }
  
  return(output)
  
}