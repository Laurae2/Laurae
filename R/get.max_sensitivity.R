#' Maximum Sensitivity (True Positive Rate)
#' 
#' This function allows to use a custom thresholding method to maximize the Sensitivity (True Positive Rate). A pair of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' 
#' @return A two element vector containing the maximum Sensitivity (True Positive Rate) for binary data, and the threshold used.
#' 
#' @export

get.max_sensitivity <- function(preds, labels) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(labels)
  numn <- length(labels) - nump
  
  DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
  DT[, tp_v := nump - cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, sens := tp_v / (tp_v + fn_v)]
  DT[, sens := ifelse(!is.finite(sens), -1, sens)]
  
  best_row <- which.max(DT$sens)
  best_sens <- DT$sens[best_row[1]]
  best_thresh <- DT$y_prob[best_row[1]]
  
  return(c(best_sens, best_thresh))
  
}