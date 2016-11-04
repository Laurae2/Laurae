#' LightGBM Model Training
#'
#' This function allows you to train a LightGBM model.
#' It is recommended to have your x_train and x_val sets as data.table, and to use the development data.table version.
#' To install data.table development version, please run in your R console: \code{install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")}.
#' The speed increase to create the train and test files can exceed 1,000x over write.table in certain cases.
#' To store evaluation metrics throughout the training, you MUST run this function with \code{verbose = FALSE}.
#' 
#' The most important parameters are \code{lgbm_path} and \code{workingdir}: they setup where LightGBM is and where temporary files are going to be stored. \code{lgbm_path} is the full path to LightGBM executable, and includes the executable name and file extension (like \code{C:/Laurae/LightGBM/windows/x64/Release/LightGBM.exe}). \code{workingdir} is the working directory for the temporary files for LightGBM. It creates a lot of necessary files to make LightGBM work (defined by \code{output_model, output_preds, train_conf, train_name, val_name, pred_conf}).
#' 
#' \code{train_conf}, \code{train_name}, and \code{val_name} defines respectively the configuration file name, the train file name, and the validation file name. They are created under this name when \code{files_exist} is set to \code{FALSE}.
#' 
#' Once you filled these variables (and if they were appropriate), you should fill \code{y_train, x_train}. If you need model validation, fill also \code{y_val, x_val}. y is your label (a vector), while x is your data.table (preferred) or a data.frame or a matrix.
#' 
#' Then, you are up to choose what you want, including hyperparameters to verbosity control.
#' 
#' To get the metric table, you MUST use \code{verbose = FALSE}. It cannot be fetched without. \code{sink()} does not work.
#' 
#' If for some reason you lose the ability to print in the console, run \code{sink()} in the console several times until you get an error.
#' 
#' @param y_train Type: vector. The training labels. Mandatory even if you put them in \code{x_train}.
#' @param x_train Type: data.table (preferred), data.frame, or matrix (unsupported). The training features.
#' @param bias_train Type: numeric or vector of numerics. The initial weights of the training data. If a numeric is provided, then the weights are identical for all the training samples. Otherwise, use the vector as weights. Defaults to \code{NA}.
#' @param y_val Type: vector. The validation labels. Defaults to \code{NA}. Mandatory even if you put them in \code{x_val}. Unused when \code{validation} is \code{TRUE}.
#' @param x_val Type: data.table (preferred), data.frame, or matrix (unsupported). The validation features. Defaults to \code{NA}. Unused when \code{validation} is \code{TRUE}.
#' @param x_test Type: data.table (preferred), data.frame, or matrix (unsupported). The testing features, if necessary. Not providing a data.frame or a matrix results in at least 3x memory usage. Defaults to \code{NA}. Predictions are averaged. Must be unlabeled.
#' @param data_has_label Type: boolean. Whether \code{x_train} and \code{x_val} have labels or not. Set this to \code{FALSE} when you already added labels to \code{x_train} and \code{x_val}. Defaults to \code{FALSE}.
#' @param header Type: boolean. Whether headers should be exported. Defaults to \code{TRUE}.
#' @param label_column Type: integer or character. The reference to the label column. If you specify a character (by name), do not specify the \code{name:} prefix as it will be appended automatically. By default, \code{ncol(x_train) + 1}.
#' @param NA_value Type: numeric or character. What value replaces NAs. Use \code{"na"} if you want to specify "missing". It is not recommended to use something else, even by soemthing like a numeric value out of bounds (like \code{-999} if all your values are greater than \code{-999}). You should change from the default \code{"na"} if they have a real numeric meaning. Defaults to \code{"na"}.
#' @param lgbm_path Type: character. Where is stored LightGBM? Include only the folder to it. Defaults to \code{'path/to/LightGBM.exe'}.
#' @param workingdir Type: character. The working directory used for LightGBM. Defaults to \code{getwd()}.
#' @param train_name Type: character. The name of the training data file for the model. Defaults to \code{'lgbm_train.csv'}.
#' @param val_name Type: character. The name of the testing data file for the model. Defaults to \code{'lgbm_val.csv'}.
#' @param test_name Type: character. The name of the testing data file for the model. Defaults to \code{'lgbm_test.csv'}.
#' @param init_score Type: string. The file name of initial (bias) training scores to start training LightGBM, which contains \code{bias_train} values. Defaults to \code{ifelse(is.na(bias_train), NA, paste(train_name, ".weight", sep = ""))}, which means \code{NA} if \code{bias_train} is left default, else appends \code{".weight"} extension to \code{train_name} name.
#' @param files_exist Type: boolean. Whether the training (and testing) files are already existing. It overwrites files if there are any existing. Defaults to \code{FALSE}.
#' @param save_binary Type: boolean. Whether data should be saved as binary files for faster load. The name takes automatically the name from the \code{train_name} and adds the extension \code{".bin"}. Defaults to \code{FALSE}.
#' @param train_conf Type: character. The name of the training configuration file for the model. Defaults to \code{'lgbm_train.conf'}.
#' @param pred_conf Type: character. The name of the prediction configuration file for the model. Defaults to \code{'lgbm_pred.conf'}.
#' @param test_conf Type: character. The name of the testing prediction configuration file for the model. Defaults to \code{'lgbm_test.conf'}.
#' @param validation Type: boolean. Whether LightGBM performs validation during the training, by outputting metrics for the validation data. Defaults to \code{ifelse(is.na(y_val), FALSE, TRUE)}, which means if \code{y_val} is the default value (unfilled), \code{validation} is \code{FALSE} else \code{TRUE}. Multi-validation data is not supported yet.
#' @param predictions Type: boolean. Should LightGBM compute predictions after training the model? Defaults to \code{FALSE}.
#' @param predict_leaf_index Type: boolean. When \code{predictions} is \code{TRUE}, should LightGBM predict leaf indexes? Defaults to \code{FALSE}. Largely recommended to keep it \code{FALSE} unless you know what you are doing.
#' @param output_preds Type: character. The file name of the prediction results for the model. Defaults to \code{'lgbm_predict_result.txt'}. Original name is \code{output_result}.
#' @param test_preds Type: character. The file name of the prediction results for the model. Defaults to \code{'lgbm_predict_test.txt'}.
#' @param verbose Type: boolean/integer. Whether to print a lot of debug messages in the console or not. 0 is FALSE and 1 is TRUE. Defaults to \code{TRUE}. When set to \code{FALSE}, the model log is output to \code{log_name} which allows to get metric information from the \code{log_name} parameter!!!
#' @param log_name Type: character. The logging (sink) file to output (like 'log.txt'). Defaults to \code{'lgbm_log.txt'}.
#' @param full_quiet Type: boolean. Whether file writing is quiet or not. When set to \code{TRUE}, the default printing is diverted to \code{'diverted_verbose.txt'}. Combined with \code{verbose = FALSE}, the function is fully quiet. Defaults to \code{FALSE}.
#' @param full_console Type: boolean. Whether a dedicated console should be visible. Defaults to \code{FALSE}.
#' @param importance Type: boolean. Should LightGBM perform feature importance? Defaults to \code{FALSE}.
#' @param output_model Type: character. The file name of output model. Defaults to \code{'lgbm_model.txt'}.
#' @param input_model Type: character. The file name of input model. If defined, LightGBM will resume training from that file. You MUST user a different \code{output_model} file name if you define \code{input_model}. Otherwise, you are overwriting your model (and if your model cannot learn by stopping immediately at the beginning, you would LOSE your model). Defaults to \code{NA}.
#' @param num_threads Type: integer. The number of threads to run for LightGBM. It is recommended to not set it higher than the amount of physical cores in your computer. Defaults to \code{2}. In virtualized environments, it can be better to set it to the maximum amount of threads allocated to the virtual machine (especially VirtualBox).
#' @param histogram_pool_size Type: integer. The maximum cache size (in MB) allocated for LightGBM histogram sketching. Values below \code{0} (like \code{-1}) means no limit. Defaults to \code{-1}.
#' @param is_sparse Type: boolean. Whether sparse optimization is enabled. When \code{TRUE}, does not allow negative values (it will set them to \code{0}). Defaults to \code{TRUE}.
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
#' @return A list with the stored trained model (\code{Model}), the path (\code{Path}) of the trained model, the name (\code{Name}) of the trained model file, the LightGBM path (\code{lgbm}) which trained the model, the training file name (\code{Train}), the validation file name even if there were none provided (\code{Valid}), the testing file name even if there were none provided (\code{Test}), the validation predictions (\code{Validation}) if \code{Predictions} is set to \code{TRUE} with a validation set, the testing predictions (\code{Testing}) if \code{Predictions} is set to \code{TRUE} with a testing set, the name of the log file \code{Log} if \code{verbose} is set to \code{FALSE}, the log file content \code{LogContent} if \code{verbose} is set to \code{FALSE}, the metrics \code{Metrics} if \code{verbose} is set to \code{FALSE}, the best iteration (\code{Best}) if \code{verbose} is set to \code{FALSE}, the column names \code{Columns} if \code{importance} is set to \code{TRUE}, and the feature importance \code{FeatureImp} if \code{importance} is set to \code{TRUE}. Returns a character variable if LightGBM is not found under lgbm_path.
#' 
#' @examples
#' \dontrun{
#' Train a regression model on 1000 samples and validate on 1000 others, with feature importance.
#' Runs on 4 threads.
#' 1000 iterations with shrinkage of 0.1, stop when 10 iterations are not increase performance.
#' 127 leaves for an "approximate equivalent" of depth = 7.
#' Uses the working dir / temp folder as working directory for the temporary files.
#' trained <- lgbm.train(y_train = targets[1:1000],
#'                       x_train = data[1:1000, ],
#'                       y_val = targets[1001:2000],
#'                       x_val = data[1001:2000, ],
#'                       lgbm_path = "C:/LightGBM/windows/x64/Release/lightgbm.exe",
#'                       workingdir = file.path(getwd(), "temp"),
#'                       validation = TRUE,
#'                       predictions = TRUE,
#'                       importance = TRUE,
#'                       num_threads = 4,
#'                       application = "regression",
#'                       learning_rate = 0.1,
#'                       num_iterations = 1000,
#'                       early_stopping_rounds = 10,
#'                       num_leaves = 127,
#'                       files_exist = FALSE)
#' }
#' 
#' @export

lgbm.train <- function(
  # Data-related
  y_train,
  x_train,
  bias_train = NA,
  y_val = NA,
  x_val = NA,
  x_test = NA,
  data_has_label = FALSE,
  header = TRUE,
  label_column = ncol(x_train) + 1,
  NA_value = "na",
  
  # LightGBM-related
  lgbm_path = 'path/to/LightGBM.exe',
  workingdir = getwd(),
  
  # Data files to create/use
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
  
  # Prediction-related
  validation = ifelse(is.na(y_val), FALSE, TRUE),
  predictions = FALSE,
  predict_leaf_index = FALSE,
  output_preds = 'lgbm_predict_result.txt',
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
  
  # Check file existance
  if(!file.exists(file.path(lgbm_path))){
    return(paste0('Could not find lightgbm.exe under ', file.path(lgbm_path), "."))
  }
  
  gc(verbose = FALSE)
  
  if (full_quiet) {
    sink(file = file.path(workingdir, "diverted_verbose.txt"), append = FALSE, split = FALSE)
  }
  
  # Attempts to speed up - Disabled temporarily
  # if (is.data.table(x_train) == FALSE) {
  #   setDT(x_train)
  #   setDT(x_val)
  # }
  
  # Setup working directory for LightGBM
  cat('Using LightGBM path: ', lgbm_path, "  \n", sep = "")
  
  # Create working directory for LightGBM
  dir.create(file.path(workingdir), showWarnings = FALSE)
  cat('Working directory of LightGBM: ', file.path(workingdir), "  \n", sep = "")
  
  # Setup the train configuration file
  #file.copy(paste0(lgbm_path), file.path(workingdir))
  fileConn <- file(file.path(workingdir, train_conf), "w")
  write(paste0('task=train'), fileConn, append = TRUE)
  write(paste0('application=', application), fileConn, append = TRUE)
  write(paste0('data="', file.path(workingdir, train_name), '"'), fileConn, append = TRUE)
  write(paste0('header=', tolower(header)))
  write(paste0('label=', ifelse(is.character(label_column), paste0("name:", label_column), label_column)))
  if (validation) write(paste0('valid="',file.path(workingdir, val_name), '"'), fileConn, append = TRUE)
  write(paste0('num_iterations=', num_iterations), fileConn, append = TRUE)
  if (!is.na(early_stopping_rounds)) write(paste0('early_stopping_rounds=', early_stopping_rounds), fileConn, append = TRUE)
  write(paste0('learning_rate=', learning_rate), fileConn, append = TRUE)
  write(paste0('num_leaves=', num_leaves), fileConn, append = TRUE)
  write(paste0('tree_learner=', tree_learner), fileConn, append = TRUE)
  write(paste0('num_threads=', num_threads), fileConn, append = TRUE)
  write(paste0('histogram_pool_size=', histogram_pool_size), fileConn, append = TRUE)
  write(paste0('min_data_in_leaf=', min_data_in_leaf), fileConn, append = TRUE)
  write(paste0('min_sum_hessian_in_leaf=', min_sum_hessian_in_leaf), fileConn, append = TRUE)
  write(paste0('feature_fraction=', feature_fraction), fileConn, append = TRUE)
  write(paste0('feature_fraction_seed=', feature_fraction_seed), fileConn, append = TRUE)
  write(paste0('bagging_fraction=', bagging_fraction), fileConn, append = TRUE)
  write(paste0('bagging_freq=', bagging_freq), fileConn, append = TRUE)
  write(paste0('bagging_seed=', bagging_seed), fileConn, append = TRUE)
  write(paste0('max_bin=', max_bin), fileConn, append = TRUE)
  write(paste0('data_random_seed=', data_random_seed), fileConn, append = TRUE)
  #write(paste0('data_has_label=', tolower(as.character(data_has_label))), fileConn, append = TRUE)
  if (output_model != '') write(paste0('output_model="', file.path(workingdir, output_model), '"'), fileConn, append = TRUE)
  if (!is.na(input_model)) write(paste0('input_model="', file.path(workingdir, input_model), '"'), fileConn, append = TRUE)
  write(paste0('is_sigmoid=', tolower(as.character(is_sigmoid))), fileConn, append = TRUE)
  if (!is.na(init_score)) write(paste0('init_score="',file.path(workingdir, init_score),'"'), fileConn, append = TRUE)
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
  cat('Training configuration file saved to: ', file.path(workingdir, train_conf), "  \n", sep = "")
  
  # Check for weights already existing but not used
  if (is.na(init_score) & file.exists(paste(file.path(workingdir, train_name), ".weight", sep = ""))) {
    cat('Renaming an existing and conflicting weight file: ', file.path(workingdir, train_name), ".weight to ", train_name, ".weight_ ...", ifelse(file.rename(paste(file.path(workingdir, train_name), ".weight", sep = ""), paste(file.path(workingdir, train_name), ".weight_", sep = "")), "Success!", "Failure! Keep going on training..."), sep = "")
  }
  
  # Export data
  if (!files_exist) {
    if (exists("fwrite") & is.data.table(x_train)) {
      # Uses the super fast CSV writer
      cat('Saving train data (data.table) file to: ', file.path(workingdir, train_name), "  \n", sep = "")
      my_data <- x_train
      if (data_has_label == FALSE) {
        my_data$datatable_target <- y_train
      }
      #setcolorder(my_data, c("datatable_target", colnames(x_train)))
      fwrite(my_data, file.path(workingdir, train_name), col.names = header, sep = ",", na = as.character(NA_value), verbose = !full_quiet, quote = FALSE)
      if (!is.na(init_score)) {
        cat('Saving train weight data (data.table) file to: ', file.path(workingdir, init_score), "  \n", sep = "")
        if (length(bias_train) == 1) {
          fwrite(data.frame(V1 = rep(bias_train, length(y_train))), file.path(workingdir, init_score), col.names = header, sep = ",", na = as.character(NA_value), verbose = !full_quiet)
        } else {
          fwrite(data.frame(V1 = bias_train), file.path(workingdir, init_score), col.names = header, sep = ",", na = as.character(NA_value), verbose = !full_quiet)
        }
      }
    } else {
      # Fallback if no fwrite
      cat('Saving train data (slow) file to: ', file.path(workingdir, train_name), "  \n", sep = "")
      if (data_has_label == TRUE) {
        write.table(x_train, file.path(workingdir, train_name), row.names = FALSE, col.names = header, sep = ',', na = as.character(NA_value))
      } else {
        write.table(cbind(x_train, datatable_target = y_train), file.path(workingdir, train_name), row.names = FALSE, col.names = header, sep = ',', na = as.character(NA_value))
      }
      gc(verbose = FALSE) # In case of memory explosion
      if (!is.na(init_score)) {
        cat('Saving train weight data (slow) file to: ', file.path(workingdir, init_score), "  \n", sep = "")
        if (length(bias_train) == 1) {
          write.table(data.frame(V1 = rep(bias_train, length(y_train))), file.path(workingdir, init_score), row.names = FALSE, col.names = header, sep = ',', na = as.character(NA_value))
        } else {
          write.table(data.frame(V1 = bias_train), file.path(workingdir, init_score), row.names = FALSE, col.names = header, sep = ',', na = as.character(NA_value))
        }
      }
    }
    if (length(x_val) > 1) {
      if (exists("fwrite") & is.data.table(x_train)) {
        cat('Saving validation data (data.table) file to: ', file.path(workingdir, val_name), "  \n", sep = "")
        my_data <- x_val
        if (data_has_label == FALSE) {
          my_data$datatable_target <- y_val
        }
        #setcolorder(my_data, c("datatable_target", colnames(x_val)))
        fwrite(my_data, file.path(workingdir, val_name), col.names = header, sep = ",", na = as.character(NA_value), verbose = !full_quiet)
      } else {
        # Fallback if no fwrite
        cat('Saving validation data (slow) file to: ', file.path(workingdir, val_name), "  \n", sep = "")
        if (data_has_label == TRUE) {
          write.table(x_val, file.path(workingdir, val_name), row.names = FALSE, col.names = header, sep = ',', na = as.character(NA_value))
        } else {
          write.table(cbind(x_val, datatable_target = y_val), file.path(workingdir, val_name), row.names = FALSE, col.names = header, sep = ',', na = as.character(NA_value))
        }
        gc(verbose = FALSE) # In case of memory explosion
      }
    }
  }
  
  cat("Starting to work on model as of ", format(Sys.time(), "%a %b %d %Y %X"), "  \n", sep = "")
  
  gc(verbose = FALSE)
  if (!verbose) {
    # Write to text file
    write.table(system(paste0('"', file.path(lgbm_path), '" config="', file.path(workingdir, train_conf), '"'), intern = TRUE), file.path(workingdir, log_name), row.names = FALSE, col.names = FALSE, quote = FALSE)
  } else {
    # Print to console
    system(paste0('"', file.path(lgbm_path), '" config="', file.path(workingdir, train_conf), '"'), intern = FALSE)
  }
  cat('Model completed, results saved in ', file.path(workingdir), "  \n", sep = "")
  
  output <- list()
  output[["Model"]] <- readLines(file.path(workingdir, output_model))
  output[["Path"]] <- file.path(workingdir)
  output[["Name"]] <- output_model
  output[["lgbm"]] <- file.path(lgbm_path)
  output[["Train"]] <- train_name
  output[["Valid"]] <- ifelse(length(x_val) == 1, NA, val_name)
  output[["Test"]] <- ifelse(length(x_test) == 1, NA, test_name)
  
  if (full_quiet) {
    sink()
  }
  
  gc(verbose = FALSE)
  if (predictions & (length(x_val) > 1)) {
    
    output[["Validation"]] <- lgbm.predict(
      model = '',
      y_pred = NA,
      x_pred = NA,
      data_has_label = TRUE,
      header = header,
      label_column = label_column,
      lgbm_path = lgbm_path,
      workingdir = workingdir,
      input_model = output_model,
      pred_conf = pred_conf,
      predict_leaf_index = predict_leaf_index,
      verbose = verbose,
      data_name = val_name,
      files_exist = TRUE,
      output_preds = output_preds,
      data.table = exists("data.table"))
  }
  
  gc(verbose = FALSE)
  if (predictions & (length(x_test) > 1)) {
    
    # Raw
    output[["Testing"]] <- lgbm.predict(
      model = '',
      y_pred = NA,
      x_pred = x_test,
      data_has_label = FALSE,
      header = header,
      label_column = label_column,
      lgbm_path = lgbm_path,
      workingdir = workingdir,
      input_model = output_model,
      pred_conf = test_conf,
      predict_leaf_index = predict_leaf_index,
      verbose = verbose,
      data_name = test_name,
      files_exist = files_exist,
      output_preds = test_preds,
      data.table = exists("data.table"))
      
    gc(verbose = FALSE)
  }
  
  
  if (!verbose) {
    output[["Log"]] <- file.path(log_name)
    output[["LogContent"]] <- readLines(file.path(output[["Path"]], output[["Log"]]))
    output[["Metrics"]] <- lgbm.metric(model = output[["LogContent"]],
                                       metrics = TRUE)
    output[["Best"]] <- lgbm.metric(model = output[["LogContent"]],
                                    metrics = FALSE)
    gc(verbose = FALSE)
  }
  
  if (importance) {
    output[["Columns"]] <- colnames(x_train)
    output[["FeatureImp"]] <- lgbm.fi(model = output, feature_names = output[["Columns"]], ntreelimit = 0)
    gc(verbose = FALSE)
  }
  
  if (!full_quiet) {
    cat("Ended to work on model as of ", format(Sys.time(), "%a %b %d %Y %X"), "  \n", sep = "")
  }
  
  return(output)
  
}