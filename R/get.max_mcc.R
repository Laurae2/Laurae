#' Maximum Matthews Correlation Coefficient
#' 
#' This function allows to use a custom thresholding method to maximize the Matthews Correlation Coefficient. A pair of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' 
#' @return A two element vector containing the maximum Matthews Correlation Coefficient for binary data, and the threshold used.
#' 
#' @export

get.max_mcc <- function(preds, labels) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(labels)
  numn <- length(labels) - nump
  
  DT[, tn_v := as.numeric(cumsum(y_true == 0))]
  DT[, fp_v := cumsum(y_true == 1)]
  DT[, fn_v := numn - tn_v]
  DT[, tp_v := nump - fp_v]
  DT <- DT[cleaner, ]
  DT[, mcc := (tp_v * tn_v - fp_v * fn_v) / sqrt((tp_v + fp_v) * (tp_v + fn_v) * (tn_v + fp_v) * (tn_v + fn_v))]
  
  best_row <- which.max(DT$mcc)
  
  if (length(best_row) > 0) {
    return(c(DT$mcc[best_row[1]], DT$y_prob[best_row[1]]))
  } else {
    return(c(-1, -1))
  }
  
}