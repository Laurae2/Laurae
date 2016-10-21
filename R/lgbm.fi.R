#' LightGBM Feature Importance
#'
#' This function allows to get the feature importance on a LightGBM model. The model file must be \code{"workingdir"}, where \code{"workingdir"} is the folder and \code{input_model} is the model file name.
#' 
#' @param model Type: list. The model file. If a character vector is provided, it is considered to be the model which is going to be saved as \code{input_model}. If a list is provided, it is used to setup to fetch the correct variables, which you can override by setting the arguments manually. If a single value is provided (like \code{NA}), then it is ignored and uses the other arguments to fetch the model locally.
#' @param workingdir Type: character. The working directory of the model file. Defaults to \code{ifelse(is.list(model), model[["Path"]], getwd())}, which means "take the model working directory if provided the model list, else take the default working directory".
#' @param input_model Type: character. The file name of the model. Defaults to \code{ifelse(is.list(model), model[["Name"]], 'lgbm_model.txt')}, which means "take the input model name if provided the model list, else take "lgbm_model.txt".
#' @param feature_names Type: vector of characters. The names of the features, in the order they were fed to LightGBM. Returns column numbers if left as \code{NA}. Defaults to \code{NA}.
#' @param ntreelimit Type: integer. The number of trees to select, starting from the first tree. Defaults to \code{0}.
#' @param data.table Type: boolean. Whether to return a data.table (\code{TRUE}) or a data.frame (\code{FALSE}). Defaults to \code{TRUE}.
#' 
#' @return A data.table (or data.frame) with 9 columns: \code{c("Feature", "Gain", "Gain_Rel_Ratio", "Gain_Abs_Ratio", "Gain_Std", "Gain_Std_Rel_Ratio", "Gain_Std_Abs_Ratio", "Freq", "Freq_Rel_Ratio", "Freq_Abs_Ratio")}
#' 
#' @examples
#' \dontrun{
#' # Feature importance on a single model without any tree limit.
#' lgbm.fi(model = trained, feature_names = colnames(data), ntreelimit = 0)
#' 
#' # Feature importance on the first model from a cross-validation without any tree limit.
#' lgbm.fi(model = trained.cv[["Models"]][[1]], feature_names = colnames(data))
#' }
#' 
#' @export

lgbm.fi <- function(
  model,
  workingdir = ifelse(is.list(model), model[["Path"]], getwd()),
  input_model = ifelse(is.list(model), model[["Name"]], "lgbm_model.txt"),
  feature_names = NA,
  ntreelimit = 0,
  data.table = TRUE) {
  
  # Export model if necessary
  if (length(model) > 1) {
    if (exists("fwrite")) {
      fwrite(as.data.table(model[["Model"]]), file.path(workingdir, input_model), col.names = FALSE, quote = FALSE)
    } else {
      write.table(model[["Model"]], file.path(workingdir, input_model), col.names = FALSE, quote = FALSE, row.names = FALSE)
    }
  }
  
  model <- readLines(file.path(workingdir, input_model))
  ntrees <- grep("Tree=", model) # Note: starts at tree=0 for iteration=1
  
  # Do tree limitation
  if ((ntreelimit > 0) & (ntreelimit < length(ntrees))) {
    model <- model[1:(ntrees[ntreelimit + 1] - 1)]
  }
  
  # Separate features
  features_list <- substr(model[grep("split_feature=", model)], 15, 9999999)
  features_list <- as.numeric(unlist(strsplit(features_list, " ")))
  
  # Separate gain
  gain_list <- substr(model[grep("split_gain=", model)], 12, 9999999)
  gain_list <- as.numeric(unlist(strsplit(gain_list, " ")))
  
  # Map scores to feature names
  if (is.na(feature_names)[1]) {
    columns <- data.frame(ID = 0:(length(feature_names) - 1), Feature = 1:length(feature_names)) # 0-indexed to 1-indexed
  } else {
    columns <- data.frame(ID = 0:(length(feature_names) - 1), Feature = feature_names, stringsAsFactors = FALSE) # 0-indexed to 1-indexed
  }
  setDT(columns)
  
  # Create data.table
  raw_table <- data.frame(ID = features_list, Gain_Sum = gain_list)
  setDT(raw_table)
  
  # Combine back ID and features
  raw_table <- merge(raw_table, columns, by = "ID", all.x = TRUE)
  raw_table[, ID := NULL]
  setcolorder(raw_table, c("Feature", "Gain_Sum"))
  
  # Compute stats
  freq_out <- raw_table[, list(Freq = length(Gain_Sum), Gain = sum(Gain_Sum), Gain_Std = sd(Gain_Sum)), by = Feature]
  freq_out$Gain_Std[is.na(freq_out$Gain_Std)] <- 0
  freq_out[, Freq_Rel_Ratio := Freq/max(Freq)]
  freq_out[, Freq_Abs_Ratio := Freq/sum(Freq)]
  freq_out[, Gain_Rel_Ratio := Gain/max(Gain)]
  freq_out[, Gain_Abs_Ratio := Gain/sum(Gain)]
  freq_out[, Gain_Std_Rel_Ratio := Gain_Std/max(Gain_Std)]
  freq_out[, Gain_Std_Abs_Ratio := Gain_Std/sum(Gain_Std)]
  setcolorder(freq_out, c("Feature", "Gain", "Gain_Rel_Ratio", "Gain_Abs_Ratio", "Gain_Std", "Gain_Std_Rel_Ratio", "Gain_Std_Abs_Ratio", "Freq", "Freq_Rel_Ratio", "Freq_Abs_Ratio"))
  freq_out <- freq_out[order(Gain, decreasing = TRUE), ]
  
  if (!data.table) {freq_out <- setDF(freq_out)}
  
  return(freq_out)
  
}