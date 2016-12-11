#' Maximum Precision (Positive Predictive Value)
#' 
#' This function allows to use a custom thresholding method to maximize the Precision (Positive Predictive Value). A pair of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' 
#' @return A two element vector containing the maximum Precision (Positive Predictive Value) for binary data, and the threshold used.
#' 
#' @export

get.max_precision <- function(preds, labels) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(y_true)
  
  DT[, fp_v := cumsum(y_true == 1)]
  DT[, tp_v := nump - fp_v]
  DT <- DT[cleaner, ]
  DT[, prec := tp_v / (tp_v + fp_v)]
  DT[, prec := ifelse(!is.finite(prec), -1, prec)]
  
  best_row <- which.max(DT$prec)
  best_prec <- DT$prec[best_row[1]]
  best_thresh <- DT$y_prob[best_row[1]]
  
  return(c(best_prec, best_thresh))
  
}