#' xgboost evaluation metric for maximum Sensitivity (True Positive Rate)
#' 
#' This function allows xgboost to use a custom thresholding method to maximize the Sensitivity (True Positive Rate). You can use this function via \code{eval_metric}. It leaks memory over time, but it can be reclaimed using \code{gc()}.
#' 
#' @param pred Type: numeric. The predictions.
#' @param dtrain Type: xgb.DMatrix. The training data.
#' 
#' @return The maximum Sensitivity (True Positive Rate) for binary data.
#' 
#' @export

xgb.max_sensitivity <- function(pred, dtrain) {
  
  y_true <- getinfo(dtrain, "label")
  
  DT <- data.table(y_true = y_true, y_prob = pred, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(y_true)
  numn <- length(y_true) - nump
  
  DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
  DT[, tp_v := nump - cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, sens := tp_v / (tp_v + fn_v)]
  DT[, sens := ifelse(!is.finite(sens), -1, sens)]
  
  best_row <- which.max(DT$sens)
  best_sens <- DT$sens[best_row[1]]
  
  return(list(metric = "sens", value = best_sens))
  
}