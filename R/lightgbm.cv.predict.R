#' LightGBM Cross-Validated Prediction
#'
#' This function allows to get cross-validated predictions on cross-validated training data. Requires a cross-validated train with \code{unicity} = TRUE.
#' 
#' @param models Type: character.The working directory of the model.
#' @param input_model Type: character. The file name of the model. Defaults to \code{'LightGBM_model.txt'}.
#' @param output_result Type: character. The output prediction file. Defaults to \code{'LightGBM_predict_result.txt'}.
#' @param lgbm_path Type: character. Where is stored LightGBM? Include only the folder to it. Defaults to \code{'path/to/LightGBM.exe'}.
#' @param val_name Type: character. The name of the testing data file (.csv) for the model. Defaults to \code{'lgbm_val'}
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
  input_model = 'LightGBM_model.txt',
  output_result = 'LightGBM_predict_result.txt',
  lgbm_path = 'path/to/LightGBM.exe',
  val_name = 'lgbm_val',
  data.table = exists("data.table")
) {
  preds <- list()
  for (i in 1:length(models)) {
    if (data.table) {
      dat <- fread(file.path(workingdir, paste0(val_name, "_", i, ".csv")))
    } else {
      dat <- read.csv(file.path(workingdir, paste0(val_name, "_", i, ".csv")))
    }
    preds[[i]]=
      lightgbm.predict(
        model = models[[i]],
        x_val = paste0(val_name, "_", i, ".csv"),
        y_val = NA,
        data_has_label = TRUE,
        input_model = input_model,
        output_result = output_result,
        lgbm_path = lgbm_path,
        files_exist = TRUE,
        data.table = data.table
      )
    
  }
  return(preds)
}
