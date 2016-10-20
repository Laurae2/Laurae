#' LightGBM Model Training
#'
#' This function allows you to train a LightGBM model.
#' It is recommended to have your x_train and x_val sets as data.table, and to use the development data.table version.
#' To install data.table development version, please run in your R console: \code{install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")}.
#' The speed increase to create the train and test files can exceed 100x over write.table in certain cases.
#' 
#' Folder/File specifics:
#' * \code{lgbm_path} is the path to LightGBM executable, and includes the executable name and tag.
#' * \code{workingdir} is the working directory for the temporary files for LightGBM. Files will be under \code{'workingdir'}.
#' * \code{train_conf}, \code{train_name}, and \code{val_name} defines respectively the configuration file name, the train file name, and the validation file name. They are created under this name when \code{files_exist} is set to \code{TRUE}.
#' 
#' @param y_train Type: vector. The training labels.
#' @param x_train Type: data.table (preferred), data.frame, or matrix. The training features.
#' @param y_val Type: vector. The validation labels. Defaults to \code{NULL}. Unused when \code{validation} is \code{TRUE}.
#' @param x_val Type: data.table (preferred), data.frame, or matrix. The validation features. Defaults to \code{NULL}. Unused when \code{validation} is \code{TRUE}.
#' @param application Type: character. The label application to learn. Must be either \code{'regression'}, \code{'binary'}, or \code{'lambdarank'}. Defaults to \code{'regression'}.
#' @param validation Type: boolean. Whether LightGBM performs validation during the training, by outputting metrics for the validation data. Defaults to \code{TRUE}. Multi-validation data is not supported yet.
#' @param num_iterations Type: integer. The number of boosting iterations LightGBM will perform. Defaults to \code{10}.
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
#' @param input_model Type: characer. The file name of input model. If defined, LightGBM will resume training from that file. Defaults to \code{'lgbm_model.txt'}. Unused yet.
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
#' @param train_conf Type: character. The name of the train_conf file (.conf) for the model. Defaults to \code{'lgbm_train'}
#' @param train_name Type: character. The name of the training data file (.csv) for the model. Defaults to \code{'lgbm_train'}
#' @param val_name Type: character. The name of the testing data file (.csv) for the model. Defaults to \code{'lgbm_val'}
#' 
#' @return The working directory for the trained model.
#' 
#' @examples 
#' None yet.
#' 
#' @export

lightgbm.train <- function(
  y_train,
  x_train,
  y_val = NULL,
  x_val = NULL,
  application = 'regression',
  validation = TRUE,
  num_iterations = 10,
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
  input_model = 'lgbm_model.txt',
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
  train_conf = 'lgbm_train',
  train_name = 'lgbm_train',
  val_name = 'lgbm_val'
) {
  
  # Check file existance
  if(!file.exists(file.path(lgbm_path))){
    return(paste0('Could not find lightgbm.exe under ', file.path(lgbm_path), "."))
  }
  
  # Setup working directory for LightGBM
  print(paste('Using LightGBM path:', lgbm_path))
  train_conf <- paste0(train_conf, ".conf")
  train_name <- paste0(train_name, ".csv")
  val_name <- paste0(val_name, ".csv")
  
  # Create working directory for LightGBM
  dir.create(file.path(workingdir), showWarnings = FALSE)
  print(paste('Working directory of LightGBM:', file.path(workingdir)))
  
  # Setup the train configuration file
  #file.copy(paste0(lgbm_path), file.path(workingdir))
  fileConn <- file(file.path(workingdir, train_conf), "w")
  write(paste0('task=train'), fileConn, append = TRUE)
  write(paste0('application=',application), fileConn, append = TRUE)
  write(paste0('data="',file.path(workingdir, train_name, '"')), fileConn, append = TRUE)
  if (validation) write(paste0('valid="',file.path(workingdir, val_name, '"')), fileConn, append = TRUE)
  write(paste0('num_iterations=', num_iterations), fileConn, append = TRUE)
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
  if (output_model!='') write(paste0('output_model="',file.path(workingdir, output_model),'"'), fileConn, append = TRUE)
  write(paste0('is_sigmoid=', tolower(as.character(is_sigmoid))), fileConn, append = TRUE)
  if (init_score!='') write(paste0('init_score="',file.path(workingdir, init_score),'"'), fileConn, append = TRUE)
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
  print(paste('Training configuration file saved to:', file.path(workingdir, train_conf)))
  
  # Export data
  if (!files_exist) {
    if (exists("fwrite") & is.data.table(x_train)) {
      # Uses the super fast CSV writer
      print(paste('Saving train data (data.table) file to:', file.path(workingdir, train_name)))
      my_data <- x_train
      my_data[, datatable_target := y_train]
      setcolorder(my_data, c("datatable_target", colnames(x_train)))
      fwrite(my_data, file.path = file.path(workingdir, train_name), col.names = FALSE, sep = ",", na = "nan")
    } else {
      # Fallback if no fwrite
      print(paste('Saving train data file to:', file.path(workingdir, train_name)))
      write.table(cbind(y_train, x_train), file.path(workingdir, train_name), row.names = FALSE, col.names = FALSE, sep = ',', na = "nan")
      gc(verbose = FALSE) # In case of memory explosion
    }
    if (validation) {
      if (exists("fwrite") & is.data.table(x_train)) {
        print(paste('Saving validation data (data.table) file to:', file.path(workingdir, val_name)))
        my_data <- x_val
        my_data[, datatable_target := y_val]
        setcolorder(my_data, c("datatable_target", colnames(x_val)))
        fwrite(my_data, file.path = file.path(workingdir, val_name), col.names = FALSE, sep = ",", na = "nan")
      } else {
        # Fallback if no fwrite
        print(paste('Saving validation data file to:', file.path(workingdir, val_name)))
        write.table(cbind(y_val, x_val), file.path(workingdir, val_name), row.names = FALSE, col.names = FALSE, sep = ',', na = "nan")
        gc(verbose = FALSE) # In case of memory explosion
      }
    }
  }
  system(paste0('"', file.path(lgbm_path), '" config="', file.path(workingdir, train_conf), '"'))
  print(paste('Model completed, results saved in ', file.path(workingdir)))
  return(workingdir)
  
}