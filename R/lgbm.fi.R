#' LightGBM Feature Importance
#'
#' This function allows to get the feature importance on a LightGBM model. The model file must be \code{"workingdir"}, where \code{"workingdir"} is the folder and \code{input_model} is the model file name.
#' 
#' @param workingdir Type: character. The working directory of the model file.
#' @param feature_names Type: vector of characters. The names of the features, in the order they were fed to LightGBM. Returns column numbers if left as \code{NA}. Defaults to \code{NA}.
#' @param input_model Type: character. The name of the model file. Defaults to \code{"lgbm_model.txt"}.
#' @param ntreelimit Type: integer. The number of trees to select, starting from the first tree. Defaults to \code{0}.
#' 
#' @return Nothing (the data.table is transformed to a data.frame already)
#' 
#' @examples
#' #None yet.
#' 
#' @export

lgbm.fi <- function(
  workingdir,
  feature_names = NA,
  input_model = "lgbm_model.txt",
  ntreelimit = 0) {
  
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
  
  return(freq_out)
  
}