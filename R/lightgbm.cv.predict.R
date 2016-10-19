#' LightGBM Cross-Validated Prediction
#'
#' This function allows to get cross-validated predictions on cross-validated training data.
#' 
#' @param model Type: character.The working directory of the model.
#' @param data_has_label Type: boolean. Whether the data has labels or not. Do not modify this. Defaults to \code{TRUE}.
#' @param input_model Type: character. The file name of the model. Defaults to \code{'LightGBM_model.txt'}.
#' @param output_result Type: character. The output prediction file. Defaults to \code{'LightGBM_predict_result.txt'}.
#' @param gbmpath Type: character. Where is stored LightGBM? Include only the folder to it. Defaults to \code{'/home/dba/KAGGLE/LightGBM'}.
#' @param data.table Type: boolean. Whether to use data.table to read data (returns a data.table). Defaults to \code{exists("data.table")}.
#' 
#' @return The predictions.
#' 
#' @examples 
#' None yet.
#' 
#' @export

lightgbm.cv.predict <- function(
  models,
  data_has_label = TRUE,
  input_model = 'LightGBM_model.txt',
  output_result = 'LightGBM_predict_result.txt',
  gbmpath = '/home/dba/KAGGLE/LightGBM',
  data.table = exists("data.table")
) {
  preds=list()
  for (i in 1:length(models)) {
    dat = read.csv(file.path(gbmpath, models[[i]], 'val.csv'))
    preds[[i]]=
      lightgbm.predict(
        models[[i]],
        y_val = dat[,1],
        x_val = dat[,-1],
        data_has_label = data_has_label,
        input_model = input_model,
        output_result = output_result,
        gbmpath = gbmpath,
        newx = FALSE,
        data.table = data.table
      )
    
  }
  return(preds)
}
