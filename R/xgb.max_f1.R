#' xgboost evaluation metric for maximum F1 Score (Precision with Sensitivity harmonic mean)
#' 
#' This function allows xgboost to use a custom thresholding method to maximize the F1 Score (Precision with Sensitivity harmonic mean). You can use this function via \code{eval_metric}. It leaks memory over time, but it can be reclaimed using \code{gc()}.
#' 
#' @param pred Type: numeric. The predictions.
#' @param dtrain Type: xgb.DMatrix. The training data.
#' 
#' @return The maximum F1 Score (Precision with Sensitivity harmonic mean) for binary data.
#' 
#' @export

xgb.max_f1 <- function(pred, dtrain) {
  
  y_true <- getinfo(dtrain, "label")
  
  DT <- data.table(y_true = y_true, y_prob = pred, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(labels)
  numn <- length(labels) - nump
  
  DT[, fp_v := cumsum(y_true == 1)]
  DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
  DT[, tp_v := nump - fp_v]
  DT <- DT[cleaner, ]
  DT[, f1s := 2 * tp_v / (2 * tp_v + fp_v + fn_v)]
  DT[, f1s := ifelse(!is.finite(f1s), -1, f1s)]
  
  best_row <- which.max(DT$f1s)
  best_f1s <- DT$f1s[best_row[1]]
  
  return(list(metric = "f1s", value = best_f1s))
  
}