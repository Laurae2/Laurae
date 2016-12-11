#' xgboost evaluation metric for maximum binary accuracy
#' 
#' This function allows xgboost to use a custom thresholding method to maximize the binary accuracy. You can use this function via \code{eval_metric}. It leaks memory over time, but it can be reclaimed using \code{gc()}.
#' 
#' @param pred Type: numeric. The predictions.
#' @param dtrain Type: xgb.DMatrix. The training data.
#' 
#' @return The maximum accuracy for binary data.
#' 
#' @export

xgb.max_acc <- function(pred, dtrain) {
  
  y_true <- getinfo(dtrain, "label")
  
  DT <- data.table(y_true = y_true, y_prob = pred, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  lens <- length(y_true)
  nump <- sum(y_true)
  
  DT[, tn_v := cumsum(y_true == 0)]
  DT[, tp_v := nump - cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, acc := (tn_v + tp_v) / lens]
  
  best_row <- which.max(DT$acc)
  if (length(best_row) > 0) {
    return(list(metric = "acc", value = DT$acc[best_row[1]]))
  } else {
    return(list(metric = "acc", value = -1))
  }
  
}