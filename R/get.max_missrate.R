#' Minimum Miss-Rate (False Negative Rate)
#' 
#' This function allows to use a custom thresholding method to minimize the minimum Miss-Rate (False Negative Rate). A pair of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' 
#' @return A two element vector containing the minimum Miss-Rate (False Negative Rate) for binary data, and the threshold used.
#' 
#' @export

get.max_missrate <- function(preds, labels) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(labels)
  numn <- length(labels) - nump
  
  DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
  DT[, tp_v := nump - cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, miss := fn_v / (tp_v + fn_v)]
  DT[, miss := ifelse(!is.finite(miss), -1, miss)]
  
  best_row <- which.min(DT$miss)
  best_miss <- DT$miss[best_row[1]]
  best_thresh <- DT$y_prob[best_row[1]]
  
  return(c(best_miss, best_thresh))
  
}