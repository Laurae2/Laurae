#' Maximum F1 Score (Precision with Sensitivity harmonic mean)
#' 
#' This function allows to use a custom thresholding method to maximize the Maximum F1 Score (Precision with Sensitivity harmonic mean). A pair of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' 
#' @return A two element vector containing the maximum Maximum F1 Score (Precision with Sensitivity harmonic mean) for binary data, and the threshold used.
#' 
#' @export

get.max_f1 <- function(preds, labels) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(labels)
  numn <- length(labels) - nump
  
  DT[, fp_v := cumsum(y_true == 1)]
  DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
  DT[, tp_v := nump - fp_v]
  DT <- DT[cleaner, ]
  DT[, f1s := 2 * tp_v / (2 * tp_v + fp_v + fn_v)]
  
  best_row <- which.max(DT$f1s)
  
  if (length(best_row) > 0) {
    return(c(DT$f1s[best_row[1]], DT$y_prob[best_row[1]]))
  } else {
    return(c(-1, -1))
  }
  
}