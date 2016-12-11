#' xgboost evaluation metric for maximum Specificity (True Negative Rate)
#' 
#' This function allows xgboost to use a custom thresholding method to maximize the Specificity (True Negative Rate) You can use this function via \code{eval_metric}. It leaks memory over time, but it can be reclaimed using \code{gc()}.
#' 
#' @param pred Type: numeric. The predictions.
#' @param dtrain Type: xgb.DMatrix. The training data.
#' 
#' @return The maximum Specificity (True Negative Rate) for binary data.
#' 
#' @export

xgb.max_specificity <- function(pred, dtrain) {
  
  y_true <- getinfo(dtrain, "label")
  
  DT <- data.table(y_true = y_true, y_prob = pred, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  
  DT[, tn_v := as.numeric(cumsum(y_true == 0))]
  DT[, fp_v := cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, spec := tn_v / (tn_v + fp_v)]
  
  best_row <- which.max(DT$spec)
  
  if (length(best_row) > 0) {
    return(list(metric = "spec", value = DT$spec[best_row[1]]))
  } else {
    return(list(metric = "spec", value = -1))
  }
  
}