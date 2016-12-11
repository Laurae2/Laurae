#' Maximum binary accuracy
#' 
#' This function allows to use a custom thresholding method to maximize the binary accuracy. A pair of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' 
#' @return A two element vector containing the maximum accuracy for binary data, and the threshold used.
#' 
#' @export

get.max_acc <- function(preds, labels) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  lens <- length(labels)
  nump <- sum(labels)
  
  DT[, tn_v := cumsum(y_true == 0)]
  DT[, tp_v := nump - cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, acc := (tn_v + tp_v) / lens]
  
  best_row <- which.max(DT$acc)
  
  if (length(best_row) > 0) {
    return(c(DT$acc[best_row[1]], DT$y_prob[best_row[1]]))
  } else {
    return(c(-1, -1))
  }
  
}