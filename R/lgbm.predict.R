#' LightGBM Prediction
#'
#' This function allows to run predictions on provided data.
#' 
#' If for some reason you lose the ability to print in the console, run \code{sink()} in the console several times until you get an error.
#' 
#' @param model Type: list. The model file. If a character vector is provided, it is considered to be the model which is going to be saved as \code{input_model}. If a list is provided, it is used to setup to fetch the correct variables, which you can override by setting the arguments manually. If a single value is provided (like \code{NA}), then it is ignored and uses the other arguments to fetch the model locally.
#' @param y_pred Type: vector. The validation labels. Leave it alone unless you know what you are doing. Defaults to \code{NA}.
#' @param x_pred Type: data.table (preferred), data.frame, or matrix. The validation features. Defaults to \code{NA}.
#' @param data_has_label Type: boolean. Whether the data has labels or not. Do not modify this. Defaults to \code{FALSE}.
#' @param lgbm_path Type: character. Where is stored LightGBM? Include only the folder to it. Defaults to \code{ifelse(is.list(model), model[["File"]], getwd())}, which means "take the model LightGBM path if provided the model list, else take the default working directory".
#' @param workingdir Type: character. The working directory used for LightGBM. Defaults to \code{ifelse(is.list(model), model[["Path"]], getwd())}, which means "take the model working directory if provided the model list, else take the default working directory".
#' @param input_model Type: character. The file name of the model. Defaults to \code{ifelse(is.list(model), model[["Name"]], 'lgbm_model.txt')}, which means "take the input model name if provided the model list, else take "lgbm_model.txt".
#' @param pred_conf Type: character. The name of the pred_conf file for the model. Defaults to \code{'lgbm_pred.conf'}
#' @param verbose Type: boolean. Whether to print to console verbose information. When FALSE, the printing is diverted to \code{"diverted_verbose.txt"}. Defaults to \code{TRUE}. Might not work when your lgbm_path has a space.
#' @param data_name Type: character. The file output name for the vaildation file. Defaults to \code{ifelse(is.list(model) & is.null(dim(x_pred)), model[["Test"]], 'lgbm_test.csv')}, which means "take the test file name if provided the model list and x_pred is left as is, else take "lgbm_test.csv". Original name is \code{val_name}.
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
  data_has_label = FALSE,
  
  # LightGBM-related
  lgbm_path = ifelse(is.list(model), model[["lgbm"]], getwd()),
  workingdir = ifelse(is.list(model), model[["Path"]], getwd()),
  
  # Model files
  input_model = ifelse(is.list(model), model[["Name"]], 'lgbm_model.txt'),
  pred_conf = 'lgbm_pred.conf',
  verbose = TRUE,
  
  # Data files
  data_name = ifelse(is.list(model) & is.null(dim(x_pred)), model[["Test"]], 'lgbm_test.csv'),
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
      fwrite(as.data.table(model[["Model"]]), file.path(workingdir, input_model), col.names = FALSE, quote = FALSE)
    } else {
      write.table(model[["Model"]], file.path(workingdir, input_model), col.names = FALSE, quote = FALSE, row.names = FALSE)
    }
  }
  
  # Export data
  if (!files_exist){
    if (exists("fwrite") & is.data.table(x_pred)) {
      # Uses the super fast CSV writer
      if (verbose) cat('Saving test data (data.table) file to: ', file.path(workingdir, data_name), "\n", sep = "")
      my_data <- x_pred
      if (length(y_pred) > 1) {
        my_data[, datatable_target := y_pred]
      }
      setcolorder(my_data, c("datatable_target", colnames(x_pred)))
      fwrite(my_data, file.path = file.path(workingdir, data_name), col.names = FALSE, sep = ",", na = "nan")
    } else {
      # Fallback if no fwrite
      if (verbose) cat('Saving test data file to: ', file.path(workingdir, data_name), "\n", sep = "")
      if (length(y_pred) > 1) {
        write.table(cbind(y_pred, x_pred), file.path(workingdir, data_name), row.names = FALSE, col.names = FALSE, sep = ',', na = "nan")
      } else {
        write.table(x_pred, file.path(workingdir, data_name), row.names = FALSE, col.names = FALSE, sep = ',', na = "nan")
      }
      gc(verbose = FALSE) # In case of memory explosion
    }
  }
  
  # Do the prediction stuff
  fileConn <- file(file.path(workingdir, pred_conf), "w")
  write(paste0('task=prediction'), fileConn, append = TRUE)
  write(paste0('data="', file.path(workingdir, data_name), '"'), fileConn, append = TRUE)
  if (input_model != '') write(paste0('input_model="', file.path(workingdir, input_model), '"'), fileConn, append = TRUE)
  if (output_preds != '') write(paste0('output_result="', file.path(workingdir, output_preds), '"'), fileConn, append = TRUE)
  write(paste0('data_has_label=', tolower(as.character(data_has_label))), fileConn, append = TRUE)
  close(fileConn)
  
  if (!verbose) sink()
  
  if (verbose) {
    system(paste0('"', file.path(lgbm_path), '" config="', file.path(workingdir, pred_conf), '"'), intern = !verbose)
  } else {
    invisible(system2(file.path(lgbm_path), args = paste0('config="', file.path(workingdir, pred_conf), '"'), stdout = file.path(workingdir, "diverted_verbose.txt")))
  }
  
  if (data.table == TRUE) {
    return(fread(file.path(workingdir, output_preds), header = FALSE)$V1)
  } else {
    return(read.csv(file.path(workingdir, output_preds), header = FALSE))
  }
}