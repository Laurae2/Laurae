#' xgboost evaluation metric for maximum Matthews Correlation Coefficient
#' 
#' This function allows xgboost to use a custom thresholding method to maximize the Matthews Correlation Coefficient. You can use this function via \code{eval_metric}. It leaks memory over time, but it can be reclaimed using \code{gc()}.
#' 
#' @param pred Type: numeric. The predictions.
#' @param dtrain Type: xgb.DMatrix. The training data.
#' 
#' @return The maximum Matthews Correlation Coefficient for binary data.
#' 
#' @export

xgb.max_mcc <- function(pred, dtrain) {
  
  y_true <- getinfo(dtrain, "label")
  
  DT <- data.table(y_true = y_true, y_prob = pred, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(y_true)
  numn <- length(y_true) - nump
  
  DT[, tn_v := as.numeric(cumsum(y_true == 0))]
  DT[, fp_v := cumsum(y_true == 1)]
  DT[, fn_v := numn - tn_v]
  DT[, tp_v := nump - fp_v]
  DT <- DT[cleaner, ]
  DT[, mcc := (tp_v * tn_v - fp_v * fn_v) / sqrt((tp_v + fp_v) * (tp_v + fn_v) * (tn_v + fp_v) * (tn_v + fn_v))]
  
  best_row <- which.max(DT$mcc)
  
  if (length(best_row) > 0) {
    return(list(metric = "mcc", value = DT$mcc[best_row[1]]))
  } else {
    return(list(metric = "mcc", value = -1))
  }
  
}