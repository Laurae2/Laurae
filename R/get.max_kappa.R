#' Maximum Kappa statistic
#' 
#' This function allows to use a custom thresholding method to maximize the Kappa statistic. A pair of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' 
#' @return A two element vector containing the maximum Kappa statistic for binary data, and the threshold used.
#' 
#' @export

get.max_kappa <- function(preds, labels) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(labels)
  numn <- length(labels) - nump
  
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
  best_thresh <- DT$y_prob[best_row[1]]
  
  return(c(best_kappa, best_thresh))
  
}