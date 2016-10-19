#' LightGBM Prediction
#'
#' This function allows to run predictions on provided data.
#' 
#' @param model Type: character. ?????
#' @param x_val Type: data.table (preferred), data.frame, or matrix. The validation features. Defaults to \code{NULL}.
#' @param y_val Type: vector. The validation labels. Defaults to \code{NULL}.
#' @param data_has_label Type: boolean. Whether the data has labels or not. Do not modify this. Defaults to \code{TRUE}.
#' @param val_name Type: character. The file output name for the vaildation file. Defaults to \code{"test.csv"}.
#' @param input_model Type: character. The file name of the model. Defaults to \code{'LightGBM_model.txt'}.
#' @param output_result Type: character. The output prediction file. Defaults to \code{'LightGBM_predict_result.txt'}.
#' @param gbmpath Type: character. Where is stored LightGBM? Include only the folder to it. Defaults to \code{'/home/dba/KAGGLE/LightGBM'}.
#' @param newx Type: boolean. Whether to NOT create CSV files for validation data, if already created. Defaults to \code{TRUE}.
#' @param data.table Type: boolean. Whether to use data.table to read data (returns a data.table). Defaults to \code{FALSE}.
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
  val_name = "test.csv",
  input_model = 'LightGBM_model.txt',
  output_result = 'LightGBM_predict_result.txt',
  gbmpath = '/home/dba/KAGGLE/LightGBM',
  newx = TRUE,
  data.table = FALSE
) {
  
  # Check file existance
  if(!file.exists(paste0(gbmpath, '/lightgbm'))){
    return(paste0('Could not find lightgbm.exe under ', paste0(gbmpath, '/lightgbm'), "."))
  }
  
  # Export data
  if (newx){
    if (exists("fwrite") & is.data.table(x_val)) {
      # Uses the super fast CSV writer
      print(paste('Saving test data (data.table) file to:', file.path(gbmpath, model, val_name)))
      my_data <- x_val
      my_data[, datatable_target := y_val]
      setcolorder(my_data, c("datatable_target", colnames(x_val)))
      fwrite(my_data, file.path = file.path(gbmpath, model, val_name), col.names = FALSE, sep = ",", na = "nan")
    } else {
      # Fallback if no fwrite
      print(paste('Saving test data file to:', file.path(gbmpath, model, val_name)))
      write.table(cbind(y_val, x_val), file.path(gbmpath, model, val_name), row.names = FALSE, col.names = FALSE, sep = ',', na = "nan")
      gc(verbose = FALSE) # In case of memory explosion
    }
  }
  
  # Do the prediction stuff
  fileConn <- file(file.path(gbmpath, model, "pred.conf"), "w")
  write(paste0('task=prediction'), fileConn, append = TRUE)
  write(paste0('data="', file.path(gbmpath, model, val_name, '"')), fileConn, append = TRUE)
  if (input_model != '') write(paste0('input_model="', file.path(gbmpath, model, input_model), '"'), fileConn, append = TRUE)
  if (output_result != '') write(paste0('output_result="', file.path(gbmpath, model, output_result), '"'), fileConn, append = TRUE)
  write(paste0('data_has_label=', tolower(as.character(data_has_label))), fileConn, append = TRUE)
  close(fileConn)
  system(paste0(file.path(gbmpath, model), '/lightgbm config=', file.path(gbmpath, model), '/pred.conf'))
  
  if (data.table == TRUE) {
    return(fread(file.path(gbmpath, model, output_result), header = FALSE))
  } else {
    return(read.csv(file.path(gbmpath, model, output_result), header = FALSE))
  }
}