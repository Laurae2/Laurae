#' xgboost evaluation metric for minimum Miss-Rate (False Negative Rate)
#' 
#' This function allows xgboost to use a custom thresholding method to minimum the Miss-Rate (False Negative Rate). You can use this function via \code{eval_metric}. It leaks memory over time, but it can be reclaimed using \code{gc()}.
#' 
#' @param pred Type: numeric. The predictions.
#' @param dtrain Type: xgb.DMatrix. The training data.
#' 
#' @return The minimum Miss-Rate (False Negative Rate) for binary data.
#' 
#' @export

xgb.max_missrate <- function(pred, dtrain) {
  
  y_true <- getinfo(dtrain, "label")
  
  DT <- data.table(y_true = y_true, y_prob = pred, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(labels)
  numn <- length(labels) - nump
  
  DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
  DT[, tp_v := nump - cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, miss := fn_v / (tp_v + fn_v)]
  
  best_row <- which.min(DT$miss)
  
  if (length(best_row) > 0) {
    return(list(metric = "miss", value = DT$miss[best_row[1]]))
  } else {
    return(list(metric = "miss", value = -1))
  }
  
}