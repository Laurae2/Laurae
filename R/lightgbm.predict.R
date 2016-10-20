#' LightGBM Prediction
#'
#' This function allows to run predictions on provided data.
#' 
#' @param model Type: character.The working directory of the model.
#' @param x_val Type: data.table (preferred), data.frame, or matrix. The validation features.
#' @param y_val Type: vector. The validation labels. Defaults to \code{NULL}.
#' @param data_has_label Type: boolean. Whether the data has labels or not. Do not modify this. Defaults to \code{TRUE}.
#' @param val_name Type: character. The file output name for the vaildation file. Defaults to \code{"lgbm_test.csv"}.
#' @param input_model Type: character. The file name of the model. Defaults to \code{'lgbm_model.txt'}.
#' @param output_result Type: character. The output prediction file. Defaults to \code{'lgbm_predict_result.txt'}.
#' @param lgbm_path Type: character. Where is stored LightGBM? Include only the folder to it. Defaults to \code{'path/to/LightGBM.exe'}.
#' @param files_exist Type: boolean. Whether to NOT create CSV files for validation data, if already created. Defaults to \code{TRUE}.
#' @param pred_conf Type: character. The name of the pred_conf file for the model. Defaults to \code{'lgbm_pred.conf'}
#' @param data.table Type: boolean. Whether to use data.table to read data (returns a data.table). Defaults to \code{exists("data.table")}.
#' 
#' @return The predictions.
#' 
#' @examples 
#' None yet.
#' 
#' @export

lightgbm.predict <- function(
  model,
  x_val,
  y_val = NA,
  data_has_label = TRUE,
  val_name = "lgbm_test.csv",
  input_model = 'lgbm_model.txt',
  output_result = 'lgbm_predict_result.txt',
  lgbm_path = 'path/to/LightGBM.exe',
  files_exist = TRUE,
  pred_conf = 'lgbm_pred',
  data.table = exists("data.table")
) {
  
  # Check file existance
  if(!file.exists(file.path(lgbm_path))){
    return(paste0('Could not find lightgbm.exe under ', file.path(lgbm_path), "."))
  }
  
  # Setup working directory for LightGBM
  pred_conf <- paste0(pred_conf, ".conf")
  
  # Export data
  if (!files_exist){
    if (exists("fwrite") & is.data.table(x_val)) {
      # Uses the super fast CSV writer
      print(paste('Saving test data (data.table) file to:', file.path(model, val_name)))
      my_data <- x_val
      my_data[, datatable_target := y_val]
      setcolorder(my_data, c("datatable_target", colnames(x_val)))
      fwrite(my_data, file.path = file.path(model, val_name), col.names = FALSE, sep = ",", na = "nan")
    } else {
      # Fallback if no fwrite
      print(paste('Saving test data file to:', file.path(model, val_name)))
      write.table(cbind(y_val, x_val), file.path(model, val_name), row.names = FALSE, col.names = FALSE, sep = ',', na = "nan")
      gc(verbose = FALSE) # In case of memory explosion
    }
  }
  
  # Do the prediction stuff
  fileConn <- file(file.path(model, pred_conf), "w")
  write(paste0('task=prediction'), fileConn, append = TRUE)
  write(paste0('data="', file.path(model, val_name, '"')), fileConn, append = TRUE)
  if (input_model != '') write(paste0('input_model="', file.path(model, input_model), '"'), fileConn, append = TRUE)
  if (output_result != '') write(paste0('output_result="', file.path(model, output_result), '"'), fileConn, append = TRUE)
  write(paste0('data_has_label=', tolower(as.character(data_has_label))), fileConn, append = TRUE)
  close(fileConn)
  system(paste0('"', file.path(lgbm_path), '" config="', file.path(model, pred_conf), '"'))
  
  if (data.table == TRUE) {
    return(fread(file.path(model, output_result), header = FALSE))
  } else {
    return(read.csv(file.path(model, output_result), header = FALSE))
  }
}