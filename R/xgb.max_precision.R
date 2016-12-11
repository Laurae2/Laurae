#' xgboost evaluation metric for maximum Precision (Positive Predictive Value)
#' 
#' This function allows xgboost to use a custom thresholding method to maximize the Precision (Positive Predictive Value). You can use this function via \code{eval_metric}. It leaks memory over time, but it can be reclaimed using \code{gc()}.
#' 
#' @param pred Type: numeric. The predictions.
#' @param dtrain Type: xgb.DMatrix. The training data.
#' 
#' @return The maximum Precision (Positive Predictive Value) for binary data.
#' 
#' @export

xgb.max_precision <- function(pred, dtrain) {
  
  y_true <- getinfo(dtrain, "label")
  
  DT <- data.table(y_true = y_true, y_prob = pred, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(y_true)
  
  DT[, fp_v := cumsum(y_true == 1)]
  DT[, tp_v := nump - fp_v]
  DT <- DT[cleaner, ]
  DT[, prec := tp_v / (tp_v + fp_v)]
  
  best_row <- which.max(DT$prec)
  
  if (length(best_row) > 0) {
    return(list(metric = "prec", value = DT$prec[best_row[1]]))
  } else {
    return(list(metric = "prec", value = -1))
  }
  
}