#' LightGBM Prediction
#'
#' This function allows to run predictions on provided data.
#' 
#' If for some reason you lose the ability to print in the console, run \code{sink()} in the console several times until you get an error.
#' 
#' @param model Type: list. The model file. If a character vector is provided, it is considered to be the model which is going to be saved as \code{input_model}. If a list is provided, it is used to setup to fetch the correct variables, which you can override by setting the arguments manually. If a single value is provided (like \code{NA}), then it is ignored and uses the other arguments to fetch the model locally.
#' @param y_pred Type: vector. The validation labels. Leave it alone unless you know what you are doing. Defaults to \code{NA}.
#' @param x_pred Type: data.table (preferred), data.frame, or dgCMatrix (with \code{SVMLight = TRUE}). The validation features. Defaults to \code{NA}.
#' @param SVMLight Type: boolean. Whether the input is a dgCMatrix to be output to SVMLight format. Setting this to \code{TRUE} enforces you must provide labels separately (in \code{y_train}) and headers will be ignored. This is default behavior of SVMLight format. Defaults to \code{FALSE}.
#' @param data_has_label Type: boolean. Whether the data has labels or not. Do not modify this. Defaults to \code{FALSE}.
#' @param lgbm_path Type: character. Where is stored LightGBM? Include only the folder to it. Defaults to \code{ifelse(is.list(model), model[["File"]], getwd())}, which means "take the model LightGBM path if provided the model list, else take the default working directory".
#' @param workingdir Type: character. The working directory used for LightGBM. Defaults to \code{ifelse(is.list(model), model[["Path"]], getwd())}, which means "take the model working directory if provided the model list, else take the default working directory".
#' @param input_model Type: character. The file name of the model. Defaults to \code{ifelse(is.list(model), model[["Name"]], 'lgbm_model.txt')}, which means "take the input model name if provided the model list, else take "lgbm_model.txt".
#' @param pred_conf Type: character. The name of the pred_conf file for the model. Defaults to \code{'lgbm_pred.conf'}.
#' @param predict_leaf_index Type: boolean. Should LightGBM predict leaf indexes instead of pure predictions? Defaults to \code{FALSE}.
#' @param verbose Type: boolean. Whether to print to console verbose information. When FALSE, the printing is diverted to \code{"diverted_verbose.txt"}. Defaults to \code{TRUE}. Might not work when your lgbm_path has a space.
#' @param data_name Type: character. The file output name for the vaildation file. Defaults to \code{ifelse(is.list(model) & is.null(dim(x_pred)), model[["Valid"]], paste('lgbm_test', ifelse(SVMLight, '.svm', '.csv')))}, which means "take the validation file name if provided the model list and x_pred is left as is, else take "lgbm_test.csv". Original name is \code{val_name}.
#' @param files_exist Type: boolean. Whether to NOT create CSV files for the prediction data, if already created. Defaults to \code{TRUE}.
#' @param output_preds Type: character. The output prediction file. Defaults to \code{'lgbm_predict_result.txt'}. Original name is \code{output_result}.
#' @param data.table Type: boolean. Whether to use data.table to read data (returns a data.table). Defaults to \code{exists("data.table")}.
#' 
#' @return The predictions as a vector.
#' 
#' @examples 
#' #None yet.
#' 
#' @export

lgbm.predict <- function(
  # Data-related
  model,
  y_pred = NA,
  x_pred = NA,
  SVMLight = FALSE,
  data_has_label = FALSE,
  
  # LightGBM-related
  lgbm_path = ifelse(is.list(model), model[["lgbm"]], getwd()),
  workingdir = ifelse(is.list(model), model[["Path"]], getwd()),
  
  # Model files
  input_model = ifelse(is.list(model), model[["Name"]], 'lgbm_model.txt'),
  pred_conf = 'lgbm_pred.conf',
  predict_leaf_index = FALSE,
  verbose = TRUE,
  
  # Data files
  data_name = ifelse(is.list(model) & is.null(dim(x_pred)), model[["Valid"]], paste('lgbm_test', ifelse(SVMLight, '.svm', '.csv'))),
  files_exist = TRUE,
  output_preds = 'lgbm_predict_result.txt',
  data.table = exists("data.table")
) {
  
  # Check file existance
  if(!file.exists(file.path(lgbm_path))){
    return(paste0('Could not find lightgbm.exe under ', file.path(lgbm_path), "."))
  }
  
  gc(verbose = FALSE)
  
  if (!verbose) sink(file = file.path(workingdir, "diverted_verbose.txt"), append = FALSE)
  
  # Export model if necessary
  if (length(model) > 1) {
    if (exists("fwrite")) {
      fwrite(as.data.table(model[["Model"]]), file.path(workingdir, input_model), col.names = FALSE, quote = FALSE, verbose = verbose)
    } else {
      write.table(model[["Model"]], file.path(workingdir, input_model), col.names = FALSE, quote = FALSE, row.names = FALSE)
    }
  }
  
  # Export data
  if (!files_exist){
    if (SVMLight) {
      if (verbose) cat('Saving test data (dgCMatrix) file to: ', file.path(workingdir, data_name), "  \n", sep = "")
      write.svmlight(x_pred, rep(0, nrow(x_pred)), file.path(workingdir, data_name))
    } else {
      if (exists("fwrite") & is.data.table(x_pred)) {
        # Uses the super fast CSV writer
        if (verbose) cat('Saving test data (data.table) file to: ', file.path(workingdir, data_name), "  \n", sep = "")
        my_data <- x_pred
        fwrite(my_data, file.path(workingdir, data_name), col.names = FALSE, sep = ",", na = "nan", verbose = verbose)
      } else {
        # Fallback if no fwrite
        if (verbose) cat('Saving test data (slow) file to: ', file.path(workingdir, data_name), "  \n", sep = "")
        write.table(x_pred, file.path(workingdir, data_name), row.names = FALSE, col.names = FALSE, sep = ',', na = "nan")
      }
    }
  }
  gc(verbose = FALSE) # In case of memory explosion
  
  # Do the prediction stuff
  fileConn <- file(file.path(workingdir, pred_conf), "w")
  write(paste0('task=prediction'), fileConn, append = TRUE)
  write(paste0('data="', file.path(workingdir, data_name), '"'), fileConn, append = TRUE)
  if (input_model != '') write(paste0('input_model="', file.path(workingdir, input_model), '"'), fileConn, append = TRUE)
  if (output_preds != '') write(paste0('output_result="', file.path(workingdir, output_preds), '"'), fileConn, append = TRUE)
  write(paste0('data_has_label=', tolower(as.character(data_has_label))), fileConn, append = TRUE)
  write(paste0('predict_leaf_index=', tolower(as.character(predict_leaf_index))), fileConn, append = TRUE)
  close(fileConn)
  
  if (!verbose) sink()
  
  
  if (!verbose) {
    # Write to text file
    write.table(system(paste0('"', file.path(lgbm_path), '" config="', file.path(workingdir, pred_conf), '"'), intern = TRUE), file.path(workingdir, "diverted_verbose.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  } else {
    # Print to console
    system(paste0('"', file.path(lgbm_path), '" config="', file.path(workingdir, pred_conf), '"'), intern = FALSE)
  }
  
  if (data.table == TRUE) {
    return(fread(file.path(workingdir, output_preds), header = FALSE)$V1)
  } else {
    return(read.csv(file.path(workingdir, output_preds), header = FALSE))
  }
}