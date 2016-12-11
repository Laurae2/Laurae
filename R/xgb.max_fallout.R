#' xgboost evaluation metric for minimum Fall-Out (False Positive Rate)
#' 
#' This function allows xgboost to use a custom thresholding method to minimize the Fall-Out (False Positive Rate). You can use this function via \code{eval_metric}. It leaks memory over time, but it can be reclaimed using \code{gc()}.
#' 
#' @param pred Type: numeric. The predictions.
#' @param dtrain Type: xgb.DMatrix. The training data.
#' 
#' @return The minimum Fall-Out (False Positive Rate) for binary data.
#' 
#' @export

xgb.max_fallout <- function(pred, dtrain) {
  
  y_true <- getinfo(dtrain, "label")
  
  DT <- data.table(y_true = y_true, y_prob = pred, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  
  DT[, tn_v := as.numeric(cumsum(y_true == 0))]
  DT[, fp_v := cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, fall := fp_v / (fp_v + tn_v)]
  
  best_row <- which.min(DT$fall)
  
  if (length(best_row) > 0) {
    return(list(metric = "fall", value = DT$fall[best_row[1]]))
  } else {
    return(list(metric = "fall", value = -1))
  }
  
}