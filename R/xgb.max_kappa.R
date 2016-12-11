#' xgboost evaluation metric for maximum Kappa statistic
#' 
#' This function allows xgboost to use a custom thresholding method to maximize the Kappa statistic. You can use this function via \code{eval_metric}. It leaks memory over time, but it can be reclaimed using \code{gc()}.
#' 
#' @param pred Type: numeric. The predictions.
#' @param dtrain Type: xgb.DMatrix. The training data.
#' 
#' @return The maximum Kappa statistic for binary data.
#' 
#' @export

xgb.max_kappa <- function(pred, dtrain) {
  
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
  DT <- DT[, pObs := (tn_v + tp_v)]
  DT <- DT[, pExp := (((tn_v + fp_v) / pObs * (tn_v + fn_v) / pObs) + ((fn_v + tp_v) / pObs + (fp_v + tp_v) / pObs))]
  DT <- DT[, kappa := (pObs - pExp) / (1 - pExp)]
  DT <- DT[, kappa := ifelse(!is.finite(kappa), -1, kappa)]
  
  best_row <- which.max(DT$kappa)
  best_kappa <- DT$kappa[best_row[1]]
  
  return(list(metric = "kappa", value = best_kappa))
  
}